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
 * Example code from:
 * https://spinalhdl.github.io/SpinalDoc-RTD/SpinalHDL/Structuring/area.html
 */
class BuiltinTimer extends Component {
  val io = new Bundle {
    val inData = in UInt(32 bits)
  }

  val timer = new Area {
    val counter = Reg(UInt(8 bit))
    val tick = counter === 0

    counter := counter - 1
    when(tick) {
      counter := 100
    }
  }

  val tickCounter = new Area {
    val value = Reg(UInt(3 bit))
    val reset = False

    when(timer.tick) {          // Refer to the tick from timer area
      value := value + 1
    }

    when(reset) {
      value := 0
    }
  }
}

object GenBuiltinTimer {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new BuiltinTimer)
  }
}

/*
 * "Use a companion object for methods and values which are not specific
 * to instances of the companion class."
 *
 * => object is like a global shared data structure.
 *    it appears there is only a single copy of that in memory
 *    but how to ensure concurrency though?
 *
 * => object could also have some global shared methods, stateless.
 */
object testObject {
  var x = 1
  def inc = x = x + 1
}

/*
 * Example code from:
 * https://spinalhdl.github.io/SpinalDoc-RTD/SpinalHDL/Structuring/area.html
 */
class TryReg extends Component {
  val io = new Bundle {
    val first = in Bool
    val tail = in Bool
  }

  println(testObject.x)
  testObject.inc
  println(testObject.x)

  // val isFirstFlit = Bool
  // val isMiddleFlit = Bool
  // val isLastFlit = Bool
  // 
  // isFirstFlit := False
  // isMiddleFlit := False
  // isLastFlit := False
  // 
  // when (io.first) {
  //   isFirstFlit := True
  // } .elsewhen (io.tail) {
  //   isLastFlit := True
  // } .otherwise {
  //   isMiddleFlit := True
  // }


  /**
   * Functional Programming works!
   */
  val foo = List(Reg(U(1,8 bits)), Reg(U(2,8 bits)), Reg(U(3,8 bits)))
  val max = foo.reduce((x, y) => x max y)

  def square_func(x: UInt): UInt = {
    x * x
  }

  /* Just two different ways to express the f */
  val square = foo.map(square_func)
  val square2 = foo.map(x => x * x)

  val sum = foo.map(_ * 1).foldLeft(U(0))(_+_)
  val sum2 = foo.map(_ * 1).foldLeft(U(0))((x, y) => x + y)


  /**
   * The following code demonstrade how to 
   * run multi-parameter functions. Same for objects.
   */
  def testMultiParam(p: Int)(f: Int => Unit): Int = {
      println(s"First parameter: $p")

      /*
       * Run the user provided function against
       * the first parameter.
       *
       * The f will be curly braced.
       */
      f(p)

      /*
       * If we are returning Int, we need this
       * If we are returning Unit, we can comment this out
       */
      return p
  }

  /*
   * 10 is the first parameter
   * { ... } is the second parameter, which is a function.
   */
  testMultiParam(10) { i =>
    println(i)
  }

  testMultiParam(8888) { x =>
    var y = x * x
    println(y)
  }

  def justPrint(p: Int): Unit = {
    println(p)
  }
  testMultiParam(6666)(justPrint)

  /*
   * The second argument is Unit type, which can be curly brace
   * protected statements. This mimic a syntax-style writing.
   */
  def my_if (cond: Boolean)(block: => Unit): Unit = {
    if (cond)
      block
  }

  my_if (true) {
      println("run")
  }

  my_if (false) {
      println("will not run")
  }
}

object GenTryReg {
  def main(args: Array[String]) {
    SpinalConfig(
      mode = Verilog,
      targetDirectory = "generated_rtl/apps/",
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = HIGH
      )
    ).generate(new TryReg)
  }
}
