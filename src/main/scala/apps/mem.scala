/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package apps

import lib._
import spinal.core._
import spinal.lib._

/*
 * Just to see how SpinalHDL handles its Mem interface.
 * it maps to `reg [width-1:0] mem [0:count-1]`
 *
 * We will use Mem quite a lot, i think.
 * Clio has more demonstration code.
 */
case class MemTester() extends Component with RenameIO {
  val io = new Bundle {
    val data_in  = in UInt(512 bits)
    val addr_in  = in UInt(10 bits)
  }

  val mem = Mem(Bits(512 bits), wordCount = 1024)

  mem.write(
    address = io.addr_in,
    data = B(io.data_in)
  )
  addPrePopTask(renameIO)
}

object GenMem {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new MemTester())
  }
}
