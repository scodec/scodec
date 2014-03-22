package scodec
package examples

import scala.concurrent.duration._
import java.io.{ File, FileInputStream }

import scodec.bits._



import scala.collection.immutable.IndexedSeq
import scalaz.{\/, -\/, \/-, Monoid}
import scalaz.std.AllInstances._
import scalaz.std.indexedSeq._
import scalaz.syntax.id._
import scalaz.syntax.monoid._
import scalaz.syntax.traverse._
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

object Performance extends App {

  val bits = {
    val b = BitVector.fromMmap(new FileInputStream(new File("/Users/mpilquist/Desktop/mpeg.pcap")).getChannel)
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
    println(f"$desc bitrate: $mbitrate%.3f Mbps - ${mbitrate/8}%.3f MBps")
    res
  }

  val pcapFile = time("pcap decode") { Codec.decodeValidValue[PcapCodec.PcapFile](bits) }
  println(s"Decoded ${pcapFile.records.size} records")


  for (i <- 1 to 20) {
  val stats = time("pcap with mpeg decode") {
    val pcapFile = Codec.decodeValidValue[PcapCodec.PcapFile](bits)
    pcapFile.records.foreach { record =>
      val mpeg = record.data.drop(22 * 8).drop(20 * 8)
      Codec.decodeValue[MpegCodecs.MpegPacket](mpeg)
    }
    //decodeMpegPacketTimestamps(pcapFile.records)
  }
  }

  def decodeMpegPacketTimestamps(records: IndexedSeq[PcapCodec.PcapRecord]): MpegPacketTimestamps = {
    records.foldMap { case record =>
      // Assume non-segmented ethernet frames and IPv4
      // Drop 22 byte ethernet frame header and 20 byte IPv4/udp header
      val mpeg = record.data.drop(22 * 8).drop(20 * 8)
      val (e, stats) = Codec.decodeAll[MpegCodecs.MpegPacket, MpegPacketTimestamps](mpeg) { p =>
        MpegPacketTimestamps.single(record.header.timestamp, p.header.pid)
      }
      stats
    }
  }

  def printStatistics(stats: Error \/ MpegPacketTimestamps) = stats match {
    case -\/(err) =>
      println("Failed to process: " + err)

    case \/-(timestamps) =>
      val (allBitrateStats, bitrateStatsByPid) = timestamps.bitrateStats
      println(s"Timespan: ${allBitrateStats.getN} seconds")
      println()
      println("Bitrate Statistics")
      println("==================")
      println()
      println("Overall")
      println("-------")

      def printBitrateStats(stats: DescriptiveStatistics) {
        val lines = IndexedSeq(
          f"  Min:     ${stats.getMin}%,.0f bps",
          f"  Max:     ${stats.getMax}%,.0f bps",
          f"  Mean:    ${stats.getMean}%,.0f bps",
          f"  StdDev:  ${stats.getStandardDeviation}%,.0f bps"
        )
        println(lines.mkString("%n".format()))
        println()
      }

      printBitrateStats(allBitrateStats)

      bitrateStatsByPid.toIndexedSeq.sortBy { case (pid, _) => pid }.foreach { case (pid, stats) =>
        println(s"PID: $pid")
        println("---------")
        printBitrateStats(stats)
      }

      println()

      val (allDeltaStats, deltaStatsByPid) = timestamps.deltaStats
      println("Time Delta Statistics")
      println("=====================")
      println()
      println("Overall")
      println("-------")

      def printDeltaStats(stats: DescriptiveStatistics) {
        val lines = IndexedSeq(
          s"  Packets: ${stats.getN + 1}",
          f"  Min:     ${stats.getMin}%,.3f ms",
          f"  Max:     ${stats.getMax}%,.3f ms",
          f"  Mean:    ${stats.getMean}%,.3f ms",
          f"  StdDev:  ${stats.getStandardDeviation}%,.3f ms"
        )
        println(lines.mkString("%n".format()))
        println()
      }
      printDeltaStats(allDeltaStats)
      deltaStatsByPid.toIndexedSeq.sortBy { case (pid, _) => pid }.foreach { case (pid, stats) =>
        println(s"PID: $pid")
        println("---------")
        printDeltaStats(stats)
      }
  }
}

case class MpegPacketTimestamps(all: IndexedSeq[Double], byPid: Map[Int, IndexedSeq[Double]]) {

  def deltaStats: (DescriptiveStatistics, Map[Int, DescriptiveStatistics]) = {

    def timestampDeltas(tss: IndexedSeq[Double]): IndexedSeq[Double] =
      (tss zip (tss drop 1)) map { case (x, y) => y - x }

    def toStats(tss: IndexedSeq[Double]): DescriptiveStatistics = {
      val stats = new DescriptiveStatistics
      timestampDeltas(tss) foreach { delta => stats.addValue(delta * 1000.0) }
      stats
    }

    (toStats(all), byPid map { case (k, v) => (k, toStats(v)) })
  }

  def bitrateStats: (DescriptiveStatistics, Map[Int, DescriptiveStatistics]) = {
    def bitrates(tss: IndexedSeq[Double]): IndexedSeq[Double] = {
      var start = tss.head
      var remaining = tss
      var bitrates = IndexedSeq.empty[Double]
      while (remaining.nonEmpty) {
        val end = start + 1
        def inSecond(ts: Double) = ts < end
        val packets = remaining takeWhile inSecond
        remaining = remaining dropWhile inSecond
        val bitrate = packets.size * 188.0 * 8
        bitrates = bitrates :+ bitrate
        start += 1
      }
      bitrates
    }

    def toStats(tss: IndexedSeq[Double]): DescriptiveStatistics = {
      val stats = new DescriptiveStatistics
      bitrates(tss) foreach { bitrate => stats.addValue(bitrate) }
      stats
    }

    (toStats(all), byPid map { case (k, v) => (k, toStats(v)) })
  }

  override def toString = s"MpegPacketTimestamps(${all.size} packets, by pid: ${byPid mapValues { _.size }})"
}

object MpegPacketTimestamps {
  def single(timestamp: Double, pid: Int): MpegPacketTimestamps = {
    val only = Vector(timestamp)
    MpegPacketTimestamps(only, Map(pid -> only))
  }

  implicit val monoidInstance: Monoid[MpegPacketTimestamps] = new Monoid[MpegPacketTimestamps] {
    def zero = MpegPacketTimestamps(IndexedSeq.empty, Map.empty)
    def append(x: MpegPacketTimestamps, y: => MpegPacketTimestamps) = MpegPacketTimestamps(x.all |+| y.all, x.byPid |+| y.byPid)
  }
}
