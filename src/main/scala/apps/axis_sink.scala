/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package apps

import lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

/*
 * This is an AXI-Stream sink, or blackhole, that does nothing.
 * It is useful if you are testing things and wish to have a valid endpoint.
 *
 * The dataWidth is configruable. Check its gen module.
 */
case class AxisSink(dataWidth: Int) extends Component with RenameIO {
  val io = new Bundle {
    val tmpConfig = AxiStreamConfig(dataWidth = dataWidth,
                                    keepWidth = dataWidth / 8,
                                    userWidth = 1)
    val in = slave Stream Fragment(AxiStreamPayload(tmpConfig))
  }

  val tdata = Reg(UInt(dataWidth bits))
  val tkeep = Reg(UInt(dataWidth/8 bits))
  val counter = Reg(UInt(8 bits)) init(0) simPublic()

  val isFirstFlit = Reg(Bool)
  val isLastFlit = Reg(Bool)

  io.in.ready := True

  isFirstFlit := False
  isLastFlit := False

  when (io.in.valid) {
    tdata := U(io.in.fragment.tdata)
    tkeep := U(io.in.fragment.tkeep)
  }

  when (io.in.isFirst) {
    isFirstFlit := True
  } .elsewhen (io.in.isLast) {
    isLastFlit := True
  }

  /*
   * fire =>
   * when both tvalid and tready are asserted
   */
  when (io.in.fire) {
    counter := counter + 1
  }

  /*
   * Rename both top-level IO signals and
   * the generated module name
   */
  addPrePopTask(renameIO)
  setDefinitionName(this.getClass.getSimpleName + "_" + dataWidth.toString)
}

object GenAxisSink {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new AxisSink(dataWidth=512))

    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new AxisSink(dataWidth=64))
  }
}

object TestAxisSink {
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(250 MHz))
    val compiled = SimConfig
      .withConfig(spinalConfig)
      .workspacePath("generated_sim/apps/")
      .withWave
      .compile(new AxisSink(dataWidth=512))

    compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(10)

        dut.clockDomain.deassertReset()
        dut.clockDomain.waitSampling(1)

        val payload = AxiStreamPayload(AxiStreamConfig(dataWidth=512))

        for (i <- 0 to 10) {
          dut.clockDomain.waitSampling(1)
          dut.io.in.valid #= true

          //payload.tdata := 40
          //dut.io.in.payload.Fragment = 
          //dut.io.in.payload_last #= true
          dut.io.in.last #= true
          dut.io.in.fragment.tdata #= 0
          dut.io.in.fragment.tkeep #= 0
          println(dut.counter.toInt)
        }
    }
  }
}
