package scodec
package examples

import scala.concurrent.duration._

import java.io.{File, FileInputStream}

import scodec.bits._

object Performance extends App {

  val bits = {
    val b = BitVector.fromMmap(new FileInputStream(new File("mpeg.pcap")).getChannel.nn)
    // Don't want the lazy IO penalty to skew results
    b.force
  }

  println(f"Size = ${(bits.size / 8 / (1024.0 * 1024.0))}%.3f MB")

  def time[A](desc: String)(f: => A): A = {
    val start = System.nanoTime
    val res = f
    val elapsed = (System.nanoTime - start)
    val elapsedInSeconds = elapsed / (1.second.toNanos.toDouble)
    val bitrate = bits.size / elapsedInSeconds
    val mbitrate = bitrate / (1024 * 1024)
    println(f"$desc bitrate: $mbitrate%.3f Mbps - ${mbitrate / 8}%.3f MBps")
    res
  }

  val pcapFile = time("pcap decode") { PcapCodec.pcapFile.decode(bits).require.value }
  println(s"Decoded ${pcapFile.records.size} records")

  for (i <- 1 to 20) {
    var pids = Set.empty[Int]
    val stats = time("pcap with mpeg decode") {
      val pcapFile = PcapCodec.pcapFile.decode(bits).require.value
      pcapFile.records.foreach { record =>
        val mpeg = record.data.drop(22 * 8).drop(20 * 8)
        MpegCodecs.packetCodec.decode(mpeg).map { result =>
          pids += result.value.header.pid
        }
      }
    }
    println("PIDs = " + pids.toList.sorted.mkString(", "))
  }
}
