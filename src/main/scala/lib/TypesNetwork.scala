/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * This file defines the network headers, e.g., ETH/IP/UDP/SuperNIC.
 */

package lib

import spinal.core._
import spinal.lib._

/* General trait shared by all header definitions */
trait NetworkHeaderTrait {
  val dataWidthBytes : Int
  val dataOffsetBytes : Int
  def dataWidthBits : Int = dataWidthBytes * 8
  def dataOffsetBits : Int = dataOffsetBytes * 8
}

object EthernetHeaders extends NetworkHeaderTrait {
  override val dataWidthBytes = 14
  override val dataOffsetBytes = 0
}

case class EthernetHeaders() extends Bundle {
  val dst_mac     = UInt(48 bits)
  val src_mac     = UInt(48 bits)
  val eth_type    = UInt(16 bits)

  val _dataWidthBits = EthernetHeaders.dataWidthBits

  override def assignFromBits(bits: Bits) = {
    assert(bits.getWidth == _dataWidthBits, "width mismatch")

    dst_mac := bits(0, 48 bits).asUInt
    src_mac := bits(48, 48 bits).asUInt
    eth_type := bits(96, 16 bits).asUInt
  }

  override def asBits = {
    val bits = Bits(_dataWidthBits bits)
    bits := 0
    bits.allowOverride
    bits(0, 48 bits) := dst_mac.asBits
    bits(48, 48 bits) := src_mac.asBits
    bits(96, 16 bits) := eth_type.asBits
    bits
  }
}

object Ipv4Headers extends NetworkHeaderTrait {
  override val dataWidthBytes = 20
  override val dataOffsetBytes = EthernetHeaders.dataWidthBytes
}

case class Ipv4Headers() extends Bundle {
  val ipv4_ihl         = UInt(4 bits)
  val ipv4_version     = UInt(4 bits)
  val ipv4_ecn         = UInt(2 bits)
  val ipv4_dscp        = UInt(6 bits)
  val ipv4_tot_len     = UInt(16 bits)
  val ipv4_id          = UInt(16 bits)
  val ipv4_frag_off    = UInt(16 bits)
  val ipv4_ttl         = UInt(8 bits)
  val ipv4_protocol    = UInt(8 bits)
  val ipv4_check       = UInt(16 bits)
  val ipv4_src_ip      = UInt(32 bits)
  val ipv4_dst_ip      = UInt(32 bits)

  val _dataWidthBits = Ipv4Headers.dataWidthBits

  override def assignFromBits(bits: Bits) = {
    assert(bits.getWidth == _dataWidthBits, "width mismatch")

    ipv4_ihl        := bits(0, 4 bits).asUInt
    ipv4_version    := bits(4, 4 bits).asUInt
    ipv4_ecn        := bits(8, 2 bits).asUInt
    ipv4_dscp       := bits(10, 6 bits).asUInt
    ipv4_tot_len    := bits(16, 16 bits).asUInt
    ipv4_id         := bits(32, 16 bits).asUInt
    ipv4_frag_off   := bits(48, 16 bits).asUInt
    ipv4_ttl        := bits(64, 8 bits).asUInt
    ipv4_protocol   := bits(72, 8 bits).asUInt
    ipv4_check      := bits(80, 16 bits).asUInt
    ipv4_src_ip     := bits(96, 32 bits).asUInt
    ipv4_dst_ip     := bits(128, 32 bits).asUInt
  }

  override def asBits = {
    val bits = Bits(_dataWidthBits bits)
    bits := 0
    bits.allowOverride
    bits(0, 4 bits)     := ipv4_ihl.asBits
    bits(4, 4 bits)     := ipv4_version.asBits
    bits(8, 2 bits)     := ipv4_ecn.asBits
    bits(10, 6 bits)    := ipv4_dscp.asBits
    bits(16, 16 bits)   := ipv4_tot_len.asBits
    bits(32, 16 bits)   := ipv4_id.asBits
    bits(48, 16 bits)   := ipv4_frag_off.asBits
    bits(64, 8 bits)    := ipv4_ttl.asBits
    bits(72, 8 bits)    := ipv4_protocol.asBits
    bits(80, 16 bits)   := ipv4_check.asBits
    bits(96, 32 bits)   := ipv4_src_ip.asBits
    bits(128,32 bits)   := ipv4_dst_ip.asBits
    bits
  }
}

object UdpHeaders extends NetworkHeaderTrait {
  override val dataWidthBytes = 8
  override val dataOffsetBytes = EthernetHeaders.dataWidthBytes +
                                 Ipv4Headers.dataWidthBytes
}

case class UdpHeaders() extends Bundle {
  val udp_src_port     = UInt(16 bits)
  val udp_dst_port     = UInt(16 bits)
  val udp_len          = UInt(16 bits)
  val udp_check        = UInt(16 bits)

  val _dataWidthBits = UdpHeaders.dataWidthBits

  override def assignFromBits(bits: Bits) = {
    assert(bits.getWidth == _dataWidthBits, "width mismatch")

    udp_src_port    := bits(0,  16 bits).asUInt
    udp_dst_port    := bits(16, 16 bits).asUInt
    udp_len         := bits(32, 16 bits).asUInt
    udp_check       := bits(48, 16 bits).asUInt
  }

  override def asBits = {
    val bits = Bits(_dataWidthBits bits)
    bits := 0
    bits.allowOverride
    bits(0,  16 bits) := udp_src_port.asBits
    bits(16, 16 bits) := udp_dst_port.asBits
    bits(32, 16 bits) := udp_len.asBits
    bits(48, 16 bits) := udp_check.asBits
    bits
  }
}

object SuperNICHeaders extends NetworkHeaderTrait {
  override val dataWidthBytes = 8
  override val dataOffsetBytes = EthernetHeaders.dataWidthBytes +
                                 Ipv4Headers.dataWidthBytes
}

/*
 * FAT NOTE:
 * If any field is changed, please update sim/Types.scala too.
 */
case class SuperNICHeaders() extends Bundle {
  /*
   * SuperNIC Transport Header
   * TODO: This is temporary
   */
  val snic_type        = UInt(8 bits)
  val snic_reserved    = UInt(8 bits)
  val snic_size        = UInt(16 bits)
  val snic_ack         = UInt(32 bits)

  val _dataWidthBits = SuperNICHeaders.dataWidthBits

  override def assignFromBits(bits: Bits) = {
    assert(bits.getWidth == _dataWidthBits, "width mismatch")

    snic_type     := bits(0, 8 bits).asUInt
    snic_reserved := bits(8, 8 bits).asUInt
    snic_size     := bits(16, 16 bits).asUInt
    snic_ack      := bits(32, 32 bits).asUInt
  }

  override def asBits = {
    val bits = Bits(_dataWidthBits bits)
    bits := 0
    bits.allowOverride
    bits(0, 8 bits)   := snic_type.asBits
    bits(8, 8 bits)   := snic_reserved.asBits
    bits(16, 16 bits) := snic_size.asBits
    bits(32, 32 bits) := snic_ack.asBits
    bits
  }
}

/*
 * NetworkHeaders is a combo of all headers above
 * Possibly a more convenient way to write, sometimes.
 */
object NetworkHeaders extends NetworkHeaderTrait {
  override val dataWidthBytes = 64
  override val dataOffsetBytes = 0
}

case class NetworkHeaders() extends Bundle {
  val dst_mac          = UInt(48 bits)
  val src_mac          = UInt(48 bits)
  val eth_type         = UInt(16 bits)

  val ipv4_ihl         = UInt(4 bits)
  val ipv4_version     = UInt(4 bits)
  val ipv4_ecn         = UInt(2 bits)
  val ipv4_dscp        = UInt(6 bits)
  val ipv4_tot_len     = UInt(16 bits)
  val ipv4_id          = UInt(16 bits)
  val ipv4_frag_off    = UInt(16 bits)
  val ipv4_ttl         = UInt(8 bits)
  val ipv4_protocol    = UInt(8 bits)
  val ipv4_check       = UInt(16 bits)
  val ipv4_src_ip      = UInt(32 bits)
  val ipv4_dst_ip      = UInt(32 bits)

  val snic_type        = UInt(8 bits)
  val snic_reserved    = UInt(8 bits)
  val snic_size        = UInt(16 bits)
  val snic_ack         = UInt(32 bits)

  val _dataWidthBits = NetworkHeaders.dataWidthBits

  override def assignFromBits(bits: Bits) = {
    assert(bits.getWidth == _dataWidthBits, "width mismatch")

    dst_mac         := bits(0, 48 bits).asUInt
    src_mac         := bits(48, 48 bits).asUInt
    eth_type        := bits(96, 16 bits).asUInt

    ipv4_ihl        := bits(112, 4 bits).asUInt
    ipv4_version    := bits(116, 4 bits).asUInt
    ipv4_ecn        := bits(120, 2 bits).asUInt
    ipv4_dscp       := bits(122, 6 bits).asUInt
    ipv4_tot_len    := bits(128, 16 bits).asUInt
    ipv4_id         := bits(144, 16 bits).asUInt
    ipv4_frag_off   := bits(160, 16 bits).asUInt
    ipv4_ttl        := bits(176, 8 bits).asUInt
    ipv4_protocol   := bits(184, 8 bits).asUInt
    ipv4_check      := bits(192, 16 bits).asUInt
    ipv4_src_ip     := bits(208, 32 bits).asUInt
    ipv4_dst_ip     := bits(240, 32 bits).asUInt

    snic_type       := bits(272, 8 bits).asUInt
    snic_reserved   := bits(280, 8 bits).asUInt
    snic_size       := bits(288, 16 bits).asUInt
    snic_ack        := bits(304, 32 bits).asUInt
  }

  override def asBits = {
    val bits = Bits(_dataWidthBits bits)
    bits := 0
    bits.allowOverride
    bits(0, 48 bits)    := dst_mac.asBits
    bits(48, 48 bits)   := src_mac.asBits
    bits(96, 16 bits)   := eth_type.asBits
    bits(112, 4 bits)   := ipv4_ihl.asBits
    bits(116, 4 bits)   := ipv4_version.asBits
    bits(120, 2 bits)   := ipv4_ecn.asBits
    bits(122, 6 bits)   := ipv4_dscp.asBits
    bits(128, 16 bits)  := ipv4_tot_len.asBits
    bits(144, 16 bits)  := ipv4_id.asBits
    bits(160, 16 bits)  := ipv4_frag_off.asBits
    bits(176, 8 bits)   := ipv4_ttl.asBits
    bits(184, 8 bits)   := ipv4_protocol.asBits
    bits(192, 16 bits)  := ipv4_check.asBits
    bits(208, 32 bits)  := ipv4_src_ip.asBits
    bits(240, 32 bits)  := ipv4_dst_ip.asBits

    bits(272, 8 bits)   := snic_type.asBits
    bits(280, 8 bits)   := snic_reserved.asBits
    bits(288, 16 bits)  := snic_size.asBits
    bits(304, 32 bits)  := snic_ack.asBits
    bits
  }

  def asBitsSwapAddr = {
    val b = asBits
    b.allowOverride
    b(0, 48 bits) := src_mac.asBits
    b(48, 48 bits) := dst_mac.asBits
    b
  }
}
