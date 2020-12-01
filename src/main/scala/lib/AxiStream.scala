/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * Some helper modules to define AXI-Stream interfaces.
 */

package lib

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._

case class AxiStreamConfig(dataWidth: Int,
                           keepWidth: Int = 0,
                           idWidth:   Int = 0,
                           destWidth: Int = 0,
                           userWidth: Int = 0) {
  def useKeep = keepWidth > 0
  def useId = idWidth > 0
  def useDest = destWidth > 0
  def useUser = userWidth > 0

  def dataBytes = dataWidth / 8
  def keepBytes = keepWidth / 8
}

/*
 * @AxiStreamPayload
 * Defines the familiar AXI4 Stream interface bundle
 * The width is controlled by @AxiStreamConfig
 */
case class AxiStreamPayload(config: AxiStreamConfig) extends Bundle {
  /*
   * Sanity checks
   * Data must be 8 bits aligned
   */
  assert(config.dataWidth % 8 == 0, "Data width is not byte aligned")
  if (config.useKeep)
    assert(config.dataWidth / 8 == config.keepWidth, "keep width mismatch")

  val tdata = Bits(config.dataWidth bits)
  val tdest = if (config.useDest) UInt(config.destWidth bits) else null
  val tkeep = if (config.useKeep) Bits(config.keepWidth bits) else null
  val tuser = if (config.useUser) Bits(config.userWidth bits) else null
  val tid   = if (config.useId)   Bits(config.idWidth bits)   else null
}

object AxiStream {
  // TODO what this is for?
  def apply(config: AxiStreamConfig, bitStream: Stream[Fragment[Bits]]): Stream[Fragment[AxiStreamPayload]] = {
    val stream = Stream Fragment AxiStreamPayload(config)

    stream.translateFrom(bitStream) { (axis, bits) =>
      axis.last := bits.last
      axis.fragment.tdata := bits.fragment
    }
    stream
  }
}
