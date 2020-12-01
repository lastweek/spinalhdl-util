/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * This file contains some general utilities used for simulation.
 */

package sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

import scodec.bits._

import java.io._

object SimUtils {

  val SuperNICTestCaseDir = "src/main/scala/supernic/test/data/"
  val AppTestCaseDir = "src/main/scala/apps/test/data/"

  /*
   * Converts bits to hex string and pad it out to the width of the bits signal
   */
  def bitsToHex(bits: Bits): String = {
    BitVector(bits.toBigInt.toByteArray).padLeft(bits.high+1).toHex
  }
}

/*
 * TestOutputCollector is used to write to output files and can also optionally write to
 * stdout
 */
class TestOutputCollector(testDir: String, testName: String, stdout: Boolean) {
  val outputFile = "generated_pkts/" + testName + "_out.txt"
  val f = new FileWriter(new File(outputFile))

  def this(testDir: String, testName: String) = this(testDir, testName, false)

  def out(fmt: String, args: Any*) {
    if (stdout) {
      printf(fmt, args:_*)
    }
    f.write(fmt.format(args:_*))
  }

  def check(): Boolean = {
    f.flush()
    val expectedFile = testDir + testName + "_exp.txt"
    val expected = scala.io.Source.fromFile(expectedFile).getLines
    val output = scala.io.Source.fromFile(outputFile).getLines

    expected.sameElements(output)
  }

  def close() = f.close()

}
