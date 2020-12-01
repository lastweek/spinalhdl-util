/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * This file contains some general utilities that are not included
 * in the spinalhdl library. Some of the code in this file are
 * adopted from Clio Util file.
 *
 * For SpinalHDL references:
 * - Stream: lib/src/main/scala/spinal/lib/Stream.scala
 */

package lib

import spinal.core._
import spinal.lib._

object Pair {
  def apply[T1 <: Data, T2 <: Data](p : (T1, T2)): Pair[T1,T2] = new Pair(p._1, p._2)
}

case class Pair[T1 <: Data, T2 <: Data](fstValue: T1, sndValue: T2) extends Bundle {
  val fst = cloneOf(fstValue)
  val snd = cloneOf(sndValue)
  fst := fstValue
  snd := sndValue
}

object Utils {

  /*
   * When @cond is true, the output value is @input.
   * When @cond is false, the output value is the value saved in reg.
   *
   * This is useful if the @input value is saved as state.
   */
  def RegNextWhenBypass[T <: Data](input: T, cond: Bool) : T = {
    val reg = RegNextWhen(input, cond)
    Mux(cond, input, reg)
  }

  /*
   * Stream.stage() will insert a stage of register, essentially a cycle.
   * foldLeft is used beautifully here.
   * 
   * We have this primitive because the original Delay() does not support
   * Stream's ready/valid signals.
   */
  def DelayStream[T <: Data](stream: Stream[T], cycles: Int) = { 
    (0 until cycles).foldLeft(stream) { (s, cycle) => s.stage() }
  }

  /*
   * The original SpinalHDL Stream libary already has a lot great helpers
    Be sure to check out its lib/src/main/scala/spinal/lib/Stream.scala
   * before you implement a new helper.
   */
  implicit class StreamUtils[T <: Data](stream: Stream[T]) {

    /*
     * This operation is concatenate two streams together as a Pair.
     * Their payload is concatenated together contiguously.
     * A Stream[Pair] is returned. Eeach individual item can be accessed
     * by using Pair.fst and Pair.snd.
     */
    def ++ [T2 <: Data](r : Stream[T2]) : Stream[Pair[T, T2]] = {
      StreamJoin.arg(stream, r).translateWith(Pair(stream.payload, r.payload))
    }
  }

  implicit class StreamFragmentUtils[T <: Data](frag: Stream[Fragment[T]]) {

    def changeFirst (f: T => Unit) : Stream[Fragment[T]] = {
      val next = cloneOf(frag)
      next.last := frag.last
      next.valid := next.valid
      next.fragment := frag.fragment
      when (frag.first) { f(next.fragment) }
      next
    }

    def fmapFirst (f : T => T) : Stream[Fragment[T]] = {
      val next = cloneOf(frag)
      next.last := frag.last
      next.valid := next.valid
      next.fragment := Mux(frag.first, f(frag.fragment), frag.fragment)
      next
    }

    // Filter message by signal
    def filterBySignal(signal : Stream[Bool]): Stream[Fragment[T]] = {
      val next = cloneOf(frag)

      next.payload := frag.payload
      next.valid := signal.payload && signal.valid && frag.valid
      frag.ready := Mux(signal.payload, signal.valid && next.ready, signal.valid)
      signal.ready := frag.lastFire
      next
    }

    def syncWith[T2 <: Data](stream : Stream[T2]): (Stream[Fragment[T]], Stream[T2]) = {
      val nextStream = cloneOf(stream)
      val nextFrag = cloneOf(frag)

      nextStream << stream.continueWhen (frag.last && frag.valid && nextFrag.ready)
      nextFrag   << frag.continueWhen   (stream.valid && Mux(frag.last, nextStream.ready, True))

      (nextFrag, nextStream)
    }

    def forkBySignal(signal : Stream[Bool]) : (Stream[Fragment[T]], Stream[Fragment[T]]) = {
      val ifTrue = cloneOf(frag)
      val ifFalse = cloneOf(frag)

      ifTrue.payload := frag.payload
      ifFalse.payload := frag.payload

      ifTrue.valid := signal.payload && signal.valid && frag.valid
      ifFalse.valid := !signal.payload && signal.valid && frag.valid

      frag.ready := signal.valid && Mux(signal.payload, ifTrue.ready, ifFalse.ready)
      signal.ready := frag.lastFire

      (ifTrue, ifFalse)
    }

    def translateWithLastInsert(data : T, requireInsert : Bool): Stream[Fragment[T]] = {
      // 1-bit state machine
      val waitInsert = Reg (Bool) init True
      val ret = cloneOf (frag)
      val doInsert = frag.last && waitInsert && requireInsert

      ret.fragment := data
      ret.last := frag.last && !doInsert
      ret.valid := frag.valid
      frag.ready := ret.ready && !doInsert

      when (frag.lastFire) { waitInsert := !waitInsert }
      ret
    }

    def liftStream[T2 <: Data](f : T => T2) : Stream[Fragment[T2]] = {
      val ret = Stream Fragment f(frag.fragment)
      ret.translateFrom (frag) { (f1, f2) =>
        f1.fragment := f(f2.fragment)
        f1.last := f2.last
      }
    }

    def demux(select: Stream[UInt], num: Int) : Seq[Stream[Fragment[T]]] = {
      val fragments = StreamDemux(frag.continueWhen(select.valid), select.payload, num)
      select.ready := frag.lastFire
      fragments
    }

    def takeFirst : Stream[Fragment[T]] = frag.takeWhen(frag.first)
  }
}
