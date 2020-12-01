/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 */

package sim

import scodec._
import scodec.bits._
import scodec.codecs._

/*
 * Those network headers are the same with the ones in lib/Types.
 * We have to define a separate class because of types..
 */
case class SimEthernetHeaders (
  dst_mac         : Long = 0,
  src_mac         : Long = 0,
  eth_type        : Int = 0
);

case class SimIpv4Headers (
  ipv4_ihl        : Int = 0,
  ipv4_version    : Int = 0,
  ipv4_ecn        : Int = 0,
  ipv4_dscp       : Int = 0,
  ipv4_tot_len    : Int = 0,
  ipv4_id         : Int = 0,
  ipv4_frag_off   : Int = 0,
  ipv4_ttl        : Int = 0,
  ipv4_protocol   : Int = 0,
  ipv4_check      : Int = 0,
  ipv4_src_ip     : Int = 0,
  ipv4_dst_ip     : Int = 0
);

case class SimUdpHeaders (
  udp_src_port    : Int = 0,
  udp_dst_port    : Int = 0,
  udp_len         : Int = 0,
  udp_check       : Int = 0
);

case class SimSuperNICHeadears (
  snic_type       : Int = 0,
  snic_reserved   : Int = 0,
  snic_size       : Int = 0,
  snic_ack        : Int = 0
);

/*
 * scodec definitions and some helpers.
 * This converts data between binary encoding and normal scala types.
 */
case object SimSerDes {
  val codecEthernetHeaders: Codec[SimEthernetHeaders] = {
    ulong(48) ::
    ulong(48) ::
    int(16)
  }.as[SimEthernetHeaders]

  val codecIpv4Headers: Codec[SimIpv4Headers] = {
    uint(4) ::
    uint(4) ::
    uint(2) ::
    uint(6) ::
    uint(16) ::
    uint(16) ::
    uint(16) ::
    uint(8) ::
    uint(8) ::
    uint(16) ::
    int(32) ::
    int(32)
  }.as[SimIpv4Headers]

  val codecUdpHeaders: Codec[SimUdpHeaders] = {
    uint(16) ::
    uint(16) ::
    uint(16) ::
    uint(16)
  }.as[SimUdpHeaders]

  val codecSuperNICHeaders: Codec[SimSuperNICHeadears] = {
    uint(8) ::
    uint(8) ::
    uint(16) ::
    int(32)
  }.as[SimSuperNICHeadears]

  val codecNetworkHeaders = (
    codecEthernetHeaders ::
    codecIpv4Headers ::
    codecSuperNICHeaders
  ).as[(SimEthernetHeaders, SimIpv4Headers, SimSuperNICHeadears)]

  def packetData(dst_mac: Long, src_mac: Long): BitVector = packetData(dst_mac, src_mac, 1)
  def packetData(dst_mac: Long, src_mac: Long, snic_type: Int) = {
    val ethHdr = SimEthernetHeaders (
      dst_mac = dst_mac,
      src_mac = src_mac
    )
    val ipv4Hdr = SimIpv4Headers()
    val udpHdr = SimUdpHeaders()
    val snicHdr = SimSuperNICHeadears(
      snic_type = snic_type
    )

    SimSerDes.codecNetworkHeaders.encode(
      ethHdr,
      ipv4Hdr,
      snicHdr).require
  }
}
