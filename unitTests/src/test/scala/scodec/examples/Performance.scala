/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec
package examples

import scala.concurrent.duration.*

import java.io.{File, FileInputStream}

import scodec.bits.*

object Performance extends App:

  val bits =
    val b = BitVector.fromMmap(new FileInputStream(new File("mpeg.pcap")).getChannel.nn)
    // Don't want the lazy IO penalty to skew results
    b.force

  println(f"Size = ${(bits.size / 8 / (1024.0 * 1024.0))}%.3f MB")

  def time[A](desc: String)(f: => A): A =
    val start = System.nanoTime
    val res = f
    val elapsed = System.nanoTime - start
    val elapsedInSeconds = elapsed / 1.second.toNanos.toDouble
    val bitrate = bits.size / elapsedInSeconds
    val mbitrate = bitrate / (1024 * 1024)
    println(f"$desc bitrate: $mbitrate%.3f Mbps - ${mbitrate / 8}%.3f MBps")
    res

  val pcapFile = time("pcap decode")(PcapCodec.pcapFile.decode(bits).require.value)
  println(s"Decoded ${pcapFile.records.size} records")

  for (i <- 1 to 20)
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
