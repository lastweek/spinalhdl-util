/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package apps

import lib._
import lib.Utils._
import spinal.core._
import spinal.lib._

case class NetFuncDummy(axisConfig: AxiStreamConfig, delayCycles: Int = 0)
  extends Component with RenameIO {

  val io = new Bundle {
    val in  = slave Stream Fragment(AxiStreamPayload(axisConfig))
    val out = master Stream Fragment(AxiStreamPayload(axisConfig))
  }

  io.out << DelayStream(io.in, delayCycles)

  addPrePopTask(renameIO)
  setDefinitionName(this.getClass.getSimpleName + "_" + axisConfig.dataWidth.toString)
}

object GenNetFuncDummy {
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
    ).generate(new NetFuncDummy(config))
  }
}
