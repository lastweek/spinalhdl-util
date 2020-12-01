/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * AXI Stream Switch.
 */

package lib

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._

/*
 * A blackbox wrapping the axis width converter.
 */
class AxiStreamWidthConverter(sconfig: AxiStreamConfig, mconfig: AxiStreamConfig)
  extends BlackBox with RenameIO {
  require(mconfig.useKeep, "Master Interface must enable tKeep!")
  require(sconfig.useKeep, "Slave Interface enable tKeep!")

  // Verilog Parameters
  val generic = new Generic {
    // Width of input AXI stream interface in bits
    val S_DATA_WIDTH = sconfig.dataWidth

    // Propagate tkeep signal on input interface
    // If disabled, tkeep assumed to be 1'b1
    val S_KEEP_ENABLE = if (sconfig.useKeep) 1 else 0

    // tkeep signal width (words per cycle) on input interface
    val S_KEEP_WIDTH = (S_DATA_WIDTH/8)

    // Width of output AXI stream interface in bits
    val M_DATA_WIDTH = mconfig.dataWidth

    // Propagate tkeep signal on output interface
    // If disabled, tkeep assumed to be 1'b1
    val M_KEEP_ENABLE = if (mconfig.useKeep) 1 else 0

    // tkeep signal width (words per cycle) on output interface
    val M_KEEP_WIDTH = (M_DATA_WIDTH/8)

    // Propagate tid signal
    val ID_ENABLE = 0

    // tid signal width
    val ID_WIDTH = 8

    // Propagate tdest signal
    val DEST_ENABLE = 0

    // tdest signal width
    val DEST_WIDTH = 8

    // Propagate tuser signal
    val USER_ENABLE = 0

    // tuser signal width
    val USER_WIDTH = 1
  }

  val io = new Bundle {
    val clk = in Bool
    val rst = in Bool

    val s_axis = slave Stream Fragment(AxiStreamPayload(sconfig))
    val m_axis = master Stream Fragment(AxiStreamPayload(mconfig))
  }

  setDefinitionName("axis_adapter")
  addPrePopTask(renameIO)
  mapClockDomain(clock = io.clk, reset = io.rst)
  addRTLPath("../lib/axis/rtl/axis_adapter.v")
}

/*
 * A very simple example module for the above AxiStreamWidthConverter blackbox.
 * Just a direct pass through connection.
 */
case class ExampleAxiStreamWidthConverter() extends Component with RenameIO {
  val in_config = AxiStreamConfig(512, keepWidth=512/8)
  val out_config = AxiStreamConfig(512, keepWidth=512/8)
  val io = new Bundle {
    val in = slave Stream Fragment(AxiStreamPayload(in_config))
    val out = master Stream Fragment(AxiStreamPayload(out_config))
  }

  val converter = new AxiStreamWidthConverter(in_config, out_config)

  converter.io.s_axis << io.in
  converter.io.m_axis >> io.out

  addPrePopTask(renameIO)
}

object GenExampleAxiStreamWidthConverter {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/lib/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new ExampleAxiStreamWidthConverter())
  }
}
