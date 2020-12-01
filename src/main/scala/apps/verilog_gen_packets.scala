/*
 * Copyright (c) 2020 Yizhou Shan, UC San Diego. All rights reserved.
 *
 * We can use this file to construct flit-based 512-bits packets.
 * It will dump the packets into an output file. The generated
 * file can be used for Verilog simulation.
 */

package apps

import sim._          /* For network scodec defines */
import supernic._     /* For supernic configuration */

import scodec._
import scodec.bits._
import scodec.codecs._

import java.io._

object VerilogSimPktGen {
  def main(args: Array[String]) = {

    val packet = SimSerDes.packetData(
      dst_mac = 0x112233445566L,
      src_mac = 0xAABBCCDDEEFFL,
      snic_type = SuperNICHeader.RequestType.NF
    )

    val packetBits = packet.padRight(ConfigSuperNIC.netWidth).reverseByteOrder

    try {
      val f = new FileWriter(new File("generated_input_packets.txt"))

      /*
       * This packet has 1 flit
       */
      f.write("1\n")
      f.write(packetBits.toHex+"\n")

      /*
       * This packet has 3 flit
       */
      f.write("3\n")
      f.write(packetBits.toHex+"\n")
      f.write(packetBits.toHex+"\n")
      f.write(packetBits.toHex+"\n")

      f.close()
    } catch {
      case ex: FileNotFoundException => {
        println("file not exist")
      }

      case ex: IOException => {
        println("IO exception")
      }
    }

    println("Finish generating packets.")
  }
}
