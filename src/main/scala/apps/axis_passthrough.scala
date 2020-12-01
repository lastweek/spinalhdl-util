/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package apps

import lib._
import spinal.core._
import spinal.lib._

case class AxisPassthrough(dataWidth: Int) extends Component with RenameIO {
  val io = new Bundle {
    val tmpConfig = AxiStreamConfig(dataWidth = dataWidth, keepWidth = dataWidth / 8)
    val in  = slave Stream Fragment(AxiStreamPayload(tmpConfig))
    val out = master Stream Fragment(AxiStreamPayload(tmpConfig))
  }

  /*
   * The first method
   */
  //io.out << io.in

  /*
   * The second method
   * Use translateFrom
   */
  io.out.translateFrom(io.in) { (to, from) =>
    to.fragment.tdata := from.fragment.tdata
    to.fragment.tkeep := from.fragment.tkeep
    to.last := from.last
  }

  addPrePopTask(renameIO)
  setDefinitionName(this.getClass.getSimpleName + "_" + dataWidth.toString)
}

object GenAxisPassthrough {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new AxisPassthrough(dataWidth=512))

    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new AxisPassthrough(dataWidth=64))
  }
}
