/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package lib

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import lib.Utils._

class AxiStreamDMACommand(addrWidth: Int, lenWidth: Int, tagWidth: Int = 0) extends Bundle {
  def useTag = tagWidth > 0

  val addr = UInt(addrWidth bits)
  val len  = UInt(lenWidth bits)
  val tag  = if (useTag) UInt(tagWidth bits) else null
}

case class AxiStreamDMAReadCommand(config: AxiStreamConfig, addrWidth: Int, lenWidth: Int, tagWidth: Int = 0)
  extends AxiStreamDMACommand(addrWidth, lenWidth, tagWidth) {
  val id   = if (config.useId)   UInt(config.idWidth bits)   else null
  val dest = if (config.useDest) UInt(config.destWidth bits) else null
  val user = if (config.useUser) UInt(config.userWidth bits) else null
}

case class AxiStreamDMAWriteCommand(addrWidth: Int, lenWidth: Int, tagWidth: Int = 0)
  extends AxiStreamDMACommand(addrWidth, lenWidth, tagWidth) {}

case class AxiStreamDMAConfig(streamConfig: AxiStreamConfig, addrWidth: Int, lenWidth: Int, tagWidth: Int = 0) {}

// The complex interface
case class AxiStreamDMAReadInterface(config: AxiStreamDMAConfig) extends Bundle with IMasterSlave {
  val cmd = Stream (AxiStreamDMAReadCommand(config.streamConfig, config.addrWidth, config.lenWidth, config.tagWidth))
  val data = Stream Fragment (Bits(512 bits))

  override def asMaster(): Unit = {
    master (cmd)
    slave (data)
  }
}

case class AxiStreamDMAWriteInterface(config: AxiStreamDMAConfig) extends Bundle with IMasterSlave {
  val cmd = Stream (AxiStreamDMAWriteCommand(config.addrWidth, config.lenWidth, config.tagWidth))
  val data = Stream Fragment (Bits(512 bits))

  override def asMaster(): Unit = {
    master (cmd)
    master (data)
  }
}

case class AxiStreamDMAInterface(config: AxiStreamDMAConfig) extends Bundle with IMasterSlave {
  val read = AxiStreamDMAReadInterface(config)
  val write = AxiStreamDMAWriteInterface(config)

  override def asMaster(): Unit = {
    master (read)
    master (write)
  }
}

// DMA Utilities
// All these applies to lower first
class AxiStreamDMAReadArbiter(num: Int, config: AxiStreamDMAConfig) extends Component {
  val dmaLatency = 64
  val io = new Bundle {
    val reads = Vec(slave (AxiStreamDMAReadInterface(config)), num)
    val dma = master (AxiStreamDMAReadInterface(config))
  }

  val arbiter = StreamArbiterFactory.lowerFirst.build(io.dma.cmd.payload, num)
  (arbiter.io.inputs, io.reads).zipped map { _ << _.cmd }
  val (cmdOut, selectStreamF) = StreamFork2(arbiter.io.output)
  io.dma.cmd << cmdOut

  // Bridge between request and return
  val selectStream = selectStreamF.translateWith (arbiter.io.chosen).queue(dmaLatency)

  // TODO: why this fix all the loops????
  val dataOuts = io.dma.data.stage().demux(selectStream, num)
  (dataOuts, io.reads).zipped map { _ >> _.data }
}

class AxiStreamDMA(axisConfig: AxiStreamConfig, axiConfig: Axi4Config, addrWidth: Int, lenWidth: Int, tagWidth: Int = 0)
  extends BlackBox with RenameIO {

  // TODO: check these width
  val generic = new Generic {
    // Width of AXI data bus in bits
    val AXI_DATA_WIDTH = axisConfig.dataWidth
    // Width of AXI address bus in bits
    val AXI_ADDR_WIDTH = addrWidth
    // Width of AXI wstrb (width of data bus in words)
    // val AXI_STRB_WIDTH = (AXI_DATA_WIDTH/8)
    // Width of AXI ID signal
    val AXI_ID_WIDTH = if (axiConfig.useId) axiConfig.idWidth else 1
    // Maximum AXI burst length to generate
    // This parameter is mainly about the max burst length
    val AXI_MAX_BURST_LEN = 16
    // Width of AXI stream interfaces in bits
    // val AXIS_DATA_WIDTH = AXI_DATA_WIDTH
    // Use AXI stream tkeep signal
    val AXIS_KEEP_ENABLE = if (axisConfig.useKeep) 1 else 0
    // AXI stream tkeep signal width (words per cycle)
    val AXIS_KEEP_WIDTH = if (axisConfig.useKeep) axisConfig.keepWidth else 1
    // Use AXI stream tlast signal
    val AXIS_LAST_ENABLE = 1
    // Propagate AXI stream tid signal
    val AXIS_ID_ENABLE = if (axisConfig.useId) 1 else 0
    // AXI stream tid signal width
    val AXIS_ID_WIDTH = if (axisConfig.useId) axisConfig.idWidth else 1
    // Propagate AXI stream tdest signal
    val AXIS_DEST_ENABLE = if (axisConfig.useDest) 1 else 0
    // AXI stream tdest signal width
    val AXIS_DEST_WIDTH = if (axisConfig.useDest) axisConfig.destWidth else 1
    // Propagate AXI stream tuser signal
    val AXIS_USER_ENABLE = if (axisConfig.useUser) 1 else 0
    // AXI stream tuser signal width
    val AXIS_USER_WIDTH = if (axisConfig.useUser) axisConfig.useUser else 1
    // Width of length field
    val LEN_WIDTH = lenWidth
    // Width of tag field
    val TAG_WIDTH = if (tagWidth > 0) tagWidth else 1
    // Enable support for scatter/gather DMA
    // (multiple descriptors per AXI stream frame)
    val ENABLE_SG = 0
    // Enable support for unaligned transfers
    val ENABLE_UNALIGNED = 1
  }

  val io = new Bundle {
    val clk = in Bool
    val rstn = in Bool

    val s_axis_read_desc  = slave Stream AxiStreamDMAReadCommand(axisConfig, addrWidth, lenWidth, tagWidth)
    val s_axis_write_desc = slave Stream AxiStreamDMAWriteCommand(addrWidth, lenWidth, tagWidth)
    val m_axis_read_data  = master Stream Fragment(AxiStreamPayload(axisConfig))
    val s_axis_write_data = slave Stream Fragment(AxiStreamPayload(axisConfig))

    // TODO: We ignore the datas
    // val m_axis_write_desc_status_id = master Flow NoData
    // val m_axis_write_desc_status_id = master Flow NoData
    val m_axi = master (Axi4(axiConfig))

    val read_enable = in Bool
    val write_enable = in Bool
    val write_abort = in Bool
  }

  setDefinitionName("AxiStreamDMA")
  addPrePopTask(renameIO)

  // TODO: check this! the inverse reset
  mapCurrentClockDomain(io.clk, io.rstn)

  addRTLPath("src/verilog-lib/axi/rtl/axi_dma.v")
  addRTLPath("src/verilog-lib/axi/rtl/axi_dma_rd.v")
  addRTLPath("src/verilog-lib/axi/rtl/axi_dma_wr.v")
}
