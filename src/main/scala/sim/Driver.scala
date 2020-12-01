/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * Drivers are used to simplify the process of generating data input
 * for simulation.
 */

package sim

import java.math.BigInteger

import lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

import scodec._

import java.io.{FileNotFoundException, IOException}

trait Driver[T <: Data] {
  val wire: T
  def tik: Boolean
  def update(update : Boolean) : Unit
  def finish: Boolean
}

/*
 * bits (byteStream and dest)
 *
 * The BitVector can be arbitray long. With tik(), a AxiStream payload
 * will constructed and sent out.
 */
class AxiStreamBitsGen(bits: (scodec.bits.BitVector, Int))(frag: Fragment[AxiStreamPayload])
  extends Driver[Fragment[AxiStreamPayload]] {

  override val wire = frag

  val config = wire.fragment.config

  var tail = bits._1
  val dest = bits._2
  def data = tail.take(config.dataWidth).padRight(config.dataWidth).reverseByteOrder.toByteArray

  override def tik = {
    val isLast = tail.sizeLessThanOrEqual(config.dataWidth)
    wire.fragment.tdata #= new BigInteger(1, data)
    wire.last #= isLast

    if (config.useDest)
      wire.fragment.tdest #= dest

    if (config.useKeep) {
      wire.fragment.tkeep #= {
        val shiftWidthF = if (isLast) tail.bytes.size % config.keepWidth else config.keepWidth
        val shiftWidth = if (shiftWidthF == 0) config.keepWidth else shiftWidthF
        (1L << shiftWidth) - 1
      }
    }
    true
  }

  override def update(update: Boolean): Unit = {
    if (update) {
      println(s"Axis: Send Data<${config.dataWidth}>: ${data.map("%02X" format _).mkString("_")}")
      tail = tail.drop(config.dataWidth)
    }
  }

  override def finish = tail.isEmpty
}

object AxiStreamBitsGen {
  def apply(seqs: bits.BitVector)(frag: Fragment[AxiStreamPayload]): AxiStreamBitsGen =
    new AxiStreamBitsGen((seqs, 0))(frag)

  def apply(seqs: bits.BitVector, tdest: Int)(frag: Fragment[AxiStreamPayload]): AxiStreamBitsGen =
    new AxiStreamBitsGen((seqs, tdest))(frag)
}

/* Generates flits from a file containing packets in hex delimited by
 * newlines
 *
 * Could throw file IO exceptions since file is accessed by constructor
 * */
class AxiStreamPktGenFromFile(fileName: String, dest: Int)(frag: Fragment[AxiStreamPayload])
  extends Driver[Fragment[AxiStreamPayload]] {

  override val wire = frag

  val config = wire.fragment.config
  var isLastFlit = false

  var pkts: Iterator[scodec.bits.BitVector] = {
    var iter = scala.io.Source.fromFile(fileName).getLines
    iter.map((s: String) => {
      try {
        val strippedString = s.replaceAll("_", "")
        val bits = strippedString.length() * 4
        scodec.bits.BitVector(BigInt(strippedString, 16).toByteArray).padLeft(bits)
      } catch {
        case ex: NumberFormatException => {
          println("ignoring none hex lines")
          scodec.bits.BitVector.empty
        }
        case _: Throwable => scodec.bits.BitVector.empty
      }
    })
  }
  var tail: scodec.bits.BitVector = nextPkt

  def data = tail.take(config.dataWidth).padLeft(config.dataWidth).toByteArray

  def nextPkt: scodec.bits.BitVector = {
    if (pkts.hasNext) {
      pkts.next
    } else {
      scodec.bits.BitVector.empty
    }
  }

  override def tik = {
    if (tail.isEmpty) {
      tail = nextPkt
    }
    isLastFlit = tail.sizeLessThanOrEqual(config.dataWidth)
    wire.fragment.tdata #= new BigInteger(1, data)
    wire.last #= isLastFlit

    if (config.useDest)
      wire.fragment.tdest #= dest

    if (config.useKeep) {
      wire.fragment.tkeep #= {
        val shiftWidthF = if (isLastFlit) tail.bytes.size % config.keepWidth else config.keepWidth
        val shiftWidth = if (shiftWidthF == 0) config.keepWidth else shiftWidthF
        (1L << shiftWidth) - 1
      }
    }
    true
  }

  override def update(update: Boolean): Unit = {
    if (update) {
      if (isLastFlit) {
        //println(s"Axis: Next Pkt Data<${config.dataWidth}>: ${data.map("%02X" format _).mkString("_")}")
        tail = nextPkt
        isLastFlit = false
      } else {
        //println(s"Axis: Send Data<${config.dataWidth}>: ${data.map("%02X" format _).mkString("_")}")
        tail = tail.drop(config.dataWidth)
      }
    }
  }

  override def finish = !pkts.hasNext && tail.isEmpty

}

object AxiStreamPktGenFromFile {
  def apply(fileName: String)(frag: Fragment[AxiStreamPayload]): AxiStreamPktGenFromFile =
    new AxiStreamPktGenFromFile(fileName, 0)(frag)

  def apply(fileName: String, tdest: Int)(frag: Fragment[AxiStreamPayload]): AxiStreamPktGenFromFile =
    new AxiStreamPktGenFromFile(fileName, tdest)(frag)
}

class StreamDriver[T <: Data](stream : Stream[T], clockDomain: ClockDomain) {

  stream.valid #= false
  stream.payload.flatten.filter(p => p != null).foreach {
    case uint: UInt => uint #= 0
    case bits: Bits => bits #= 0
    case bool: Bool => bool #= false
  }

  /*
   * genFunc is a multi-parameter function. It already comes with an parameter,
   * which is the T. Within this "=#", we will pass a second parameter,
   * which is stream.payload. For instance, if you look at one of the
   * genFunc, the AxiStreamBitsGen, you will notice it has two parameter lists.
   *
   * #= is Arity-1 (Infix Notation) function
   * https://docs.scala-lang.org/style/method-invocation.html
   * Therefore it can be invoked without using dot and parentheses.
   */
  def #= (genFunc: T => Driver[T]) = {
    val driver = genFunc(stream.payload)

    while (!driver.finish) {
      stream.valid #= driver.tik
      clockDomain.waitFallingEdge
      driver.update(stream.ready.toBoolean && stream.valid.toBoolean)
      clockDomain.waitRisingEdge
    }
    stream.valid #= false
  }
}
