/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * This is simple AXI-Stream packet generator.
 */

package apps

import lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

/*
 * @enable: Enable the packet generation when asserted
 * @out: the output AXI-Stream
 *
 * Once enabled, this module will generate packets at a fixed frequency.
 * For example, one packet a second. It is configurable.
 *
 * The number of flits of each packets are random. But there is a max
 * which we can control below.
 */
case class AxisPktGen(dataWidth: Int) extends Component with RenameIO {
  val io = new Bundle {
    val tmpConfig = AxiStreamConfig(dataWidth = dataWidth,
                                    keepWidth = dataWidth / 8,
                                    userWidth = 1)
    val enable = in Bool
    val out = master Stream Fragment(AxiStreamPayload(tmpConfig))
  }

  io.out.last := False
  io.out.valid := False
  io.out.fragment.tdata := 0
  io.out.fragment.tkeep := 0
  io.out.fragment.tuser := 0

  val timeout = Timeout(2000 ns)

  when (timeout && io.enable) {
    timeout.clear()
    when (io.enable & io.out.ready) {
      io.out.valid := True
      io.out.fragment.tdata := 0
      io.out.fragment.tkeep := B(dataWidth/8 bits, default -> True)
      io.out.last := True
    }
  }

  addPrePopTask(renameIO)
  setDefinitionName(this.getClass.getSimpleName + "_" + dataWidth.toString)
}

object GenAxisPktGen {
  def main(args: Array[String]) {
    val freq = 250 MHz

    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultClockDomainFrequency = FixedFrequency(freq),
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new AxisPktGen(dataWidth=512))

    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultClockDomainFrequency = FixedFrequency(freq),
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new AxisPktGen(dataWidth=64))
  }
}

object TestAxisPktGen {
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(250 MHz))
    val compiled = SimConfig
      .withConfig(spinalConfig)
      .workspacePath("generated_sim/apps/")
      .withWave
      .compile(new AxisPktGen(dataWidth=64))

    compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(5)

        dut.clockDomain.assertReset()
        dut.clockDomain.waitRisingEdge(5)
        dut.clockDomain.deassertReset()

        dut.clockDomain.waitRisingEdge(5)
        
        dut.io.enable #= true
        dut.io.out.ready #= true

        for (i <- 0 to 1000) {
          dut.clockDomain.waitRisingEdge()

          //println(dut.counter.toBigInt)
          //println(dut.nr_flit.toInt)
        }
    }
  }
}
