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
 * This is the public AxiStreamSwitch module.
 *
 * Both the slave and master ports are using the same axis configuration,
 * therefore same width and others.
 *
 * This is the most basic building block. You can use this to build
 * all sorts of AXIS Interconnect, e.g., width convertion, adding FIFOs.
 */
case class AxiStreamSwitch(scount: Int, mcount: Int, config: AxiStreamConfig)
  extends BlackBox with RenameIO {

  require(config.useKeep, "AxiSwitch requires TKEEP")
  require(config.useDest, "AxiSwitch requires TDEST")
  require(scount > 1, "AxiSwitch must have more than one slave port")
  require(mcount > 1, "AxiSwitch must have more than one master port")

  val generic = new Generic {
    // Width of AXI stream interfaces in bits
    val DATA_WIDTH = config.dataWidth

    // Propagate tkeep signal
    val KEEP_ENABLE = 1
    val KEEP_WIDTH  = (DATA_WIDTH/8)

    // Propagate tid signal
    val ID_ENABLE = if (config.useId) 1 else 0
    val ID_WIDTH  = if (config.useId) config.idWidth else 0

    // tdest signal width
    // must be wide enough to uniquely address outputs
    val DEST_WIDTH = config.destWidth

    // Propagate tuser signal
    val USER_ENABLE = if (config.useUser) 1 else 0
    val USER_WIDTH  = if (config.useUser) config.userWidth else 0

    /*
     * TODO
     * Check if we need to tune the following few configs
     */

    // Input interface register type
    // 0 to bypass, 1 for simple buffer, 2 for skid buffer
    val S_REG_TYPE = 0

    // Output interface register type
    // 0 to bypass, 1 for simple buffer, 2 for skid buffer
    val M_REG_TYPE = 2

    // arbitration type: "PRIORITY" or "ROUND_ROBIN"
    val ARB_TYPE = "ROUND_ROBIN"

    // LSB priority: "LOW", "HIGH"
    val LSB_PRIORITY = "HIGH"
  }

  val io = new Bundle {
    val clk = in Bool
    val rst = in Bool

    val s_axis = Vec(slave Stream Fragment(AxiStreamPayload(config)), scount)
    val m_axis = Vec(master Stream Fragment(AxiStreamPayload(config)), mcount)
  }

  setDefinitionName("axis_switch_wrap_" + scount.toString + "x" + mcount.toString)
  addPrePopTask(renameIO)
  mapClockDomain(clock = io.clk, reset = io.rst)

  /*
   * XXX
   *
   * This axis switch use an arbiter that has 2-cycle delay, which cannot be pipelined.
   * In the worst case, if we are sending 1-flit packet, we will only be able to send
   * 1 packet per 3 cycles. Basically, smaller the packet, worse the output bandwidth.
   * 
   * This of course is not acceptable. We need to find alternative solution.
   */
  addRTLPath("../lib/axis/rtl/axis_switch_wrap_" + scount.toString + "x" + mcount.toString + ".v")
  addRTLPath("../lib/axis/rtl/axis_switch.v")
  addRTLPath("../lib/axis/rtl/arbiter.v")
  addRTLPath("../lib/axis/rtl/axis_register.v")
  addRTLPath("../lib/axis/rtl/priority_encoder.v")
}

/*
 * This is an example demonstrating the usage of AxiStreamSwitch.
 * Here we connect inputs/outputs directly to the switch.
 */
case class ExampleAxiStreamSwitch(scount: Int, mcount: Int) extends Component with RenameIO {

  /* We must have dest, and it should be based on number of master ports. */
  val config = AxiStreamConfig(512, keepWidth=512/8, destWidth=log2Up(mcount),
                               idWidth = 1, userWidth = 1)

  val io = new Bundle {
    val in = Vec(slave Stream Fragment(AxiStreamPayload(config)), scount)
    val out = Vec(master Stream Fragment(AxiStreamPayload(config)), mcount)
  }

  val switch = AxiStreamSwitch(scount, mcount, config)

  for (i <- 0 to (scount-1))
    switch.io.s_axis(i) << io.in(i)

  for (i <- 0 to (mcount-1))
    switch.io.m_axis(i) >> io.out(i)

  setDefinitionName(this.getClass.getSimpleName + "_" + scount.toString + "x" + mcount.toString)
  addPrePopTask(renameIO)
}

/*
 * This is the code to test the example AxiStreamSwitch
 */
object TestExampleAxiStreamSwitch {
  def main(args: Array[String]) {
    val scount = 2
    val mcount = 2

    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(250 MHz))
    val compiled = SimConfig
      .withConfig(spinalConfig)
      .workspacePath("generated_sim/lib/")
      .withWave
      .compile(new ExampleAxiStreamSwitch(scount=scount, mcount=mcount))

    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(5)

      dut.clockDomain.assertReset()
      dut.clockDomain.waitRisingEdge(5)
      dut.clockDomain.deassertReset()

      dut.io.in(1).valid #= false
      dut.io.in(1).last #= false
      dut.io.out(0).ready #= true
      dut.io.out(1).ready #= true

      for (i <- 0 to 20) {
        dut.io.in(0).fragment.tdest #= BigInt("1",16)
        dut.io.in(0).valid #= true
        dut.io.in(0).last #= true
        dut.clockDomain.waitRisingEdge(1)
      }

    }
  }
}

object GenExampleAxiStreamSwitch {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/lib/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new ExampleAxiStreamSwitch(scount=2, mcount=4))

    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/lib/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new ExampleAxiStreamSwitch(scount=3, mcount=4))
  }
}
