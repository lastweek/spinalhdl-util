/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * This file includes a dummy transport module for traffic passthrough
 * and with a configurable delay in cycles.
 */

package apps

import lib._
import sim._
import lib.Utils._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._

case class TransportDummy(axisConfig: AxiStreamConfig, delayCycles: Int = 0)
  extends Component with RenameIO {

  val io = new Bundle {
    val in  = slave Stream Fragment(AxiStreamPayload(axisConfig))
    val out = master Stream Fragment(AxiStreamPayload(axisConfig))
  }

  io.out << DelayStream(io.in, delayCycles)

  addPrePopTask(renameIO)
  setDefinitionName(this.getClass.getSimpleName + "_" + axisConfig.dataWidth.toString)
}

object GenTransportDummy {
  def main(args: Array[String]) {

    val netWidth = 512
    def config = AxiStreamConfig(
      dataWidth = netWidth,
      keepWidth = netWidth / 8,
      destWidth = 16,
      userWidth = 1
    )

    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new TransportDummy(config))
  }
}

object TestTransportDummy {
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(250 MHz))

    val netWidth = 512
    def config = AxiStreamConfig(
      dataWidth = netWidth,
      keepWidth = netWidth / 8,
      destWidth = 16,
      userWidth = 1
    )

    val delayCycles = 4

    val compiled = SimConfig
      .withConfig(spinalConfig)
      .workspacePath("generated_sim/apps/")
      .withWave
      .compile(new TransportDummy(config, delayCycles))

    compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(5)

        dut.io.in.valid #= false

        dut.clockDomain.assertReset()
        dut.clockDomain.waitRisingEdge(5)
        dut.clockDomain.deassertReset()
        dut.clockDomain.waitRisingEdge(5)

        dut.io.in.valid #= true
        dut.io.in.fragment.tdata #= 1
        dut.clockDomain.waitRisingEdge(1)
        dut.io.in.valid #= false
        dut.clockDomain.waitRisingEdge(delayCycles + 1)

        fork {
          dut.io.out.ready #= true
        }
    }
  }
}
