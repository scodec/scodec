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

import scodec.bits.BitVector

/**
  * Processes libpcap files.
  *
  * @see http://wiki.wireshark.org/Development/LibpcapFileFormat
  */
object PcapCodec:
  import scodec.codecs.*

  enum ByteOrdering { case BigEndian, LittleEndian }

  private val magicNumber = 0x000000A1B2C3D4L
  val byteOrdering = "magic_number" | Codec[ByteOrdering](
    (bo: ByteOrdering) =>
      if (bo == ByteOrdering.BigEndian) uint32.encode(magicNumber) else uint32L.encode(magicNumber),
    (buf: BitVector) =>
      uint32.decode(buf).map {
        _.map { mn =>
          if (mn == magicNumber) ByteOrdering.BigEndian else ByteOrdering.LittleEndian
        }
      }
  )

  def gint16(using ordering: ByteOrdering): Codec[Int] =
    if (ordering == ByteOrdering.BigEndian) int16 else int16L
  def guint16(using ordering: ByteOrdering): Codec[Int] =
    if (ordering == ByteOrdering.BigEndian) uint16 else uint16L
  def gint32(using ordering: ByteOrdering): Codec[Int] =
    if (ordering == ByteOrdering.BigEndian) int32 else int32L
  def guint32(using ordering: ByteOrdering): Codec[Long] =
    if (ordering == ByteOrdering.BigEndian) uint32 else uint32L

  case class PcapHeader(
      ordering: ByteOrdering,
      versionMajor: Int,
      versionMinor: Int,
      thiszone: Int,
      sigfigs: Long,
      snaplen: Long,
      network: Long
  )

  val pcapHeader = {
    ("magic_number" | byteOrdering).flatPrepend { implicit ordering =>
      ("version_major" | guint16) ::
        ("version_minor" | guint16) ::
        ("thiszone" | gint32) ::
        ("sigfigs" | guint32) ::
        ("snaplen" | guint32) ::
        ("network" | guint32)
    }
  }.as[PcapHeader]

  case class PcapRecordHeader(
      timestampSeconds: Long,
      timestampMicros: Long,
      includedLength: Long,
      originalLength: Long
  ):
    def timestamp: Double = timestampSeconds + (timestampMicros / (1.second.toMicros.toDouble))

  def pcapRecordHeader(using ordering: ByteOrdering) = {
    ("ts_sec" | guint32) ::
      ("ts_usec" | guint32) ::
      ("incl_len" | guint32) ::
      ("orig_len" | guint32)
  }.as[PcapRecordHeader]

  case class PcapRecord(header: PcapRecordHeader, data: BitVector)

  def pcapRecord(using ordering: ByteOrdering) = {
    ("record_header" | pcapRecordHeader).flatZip { hdr =>
      ("record_data" | bits(hdr.includedLength * 8))
    }
  }.as[PcapRecord]

  case class PcapFile(header: PcapHeader, records: Vector[PcapRecord])

  val pcapFile = pcapHeader.flatZip(hdr => vector(pcapRecord(using hdr.ordering))).as[PcapFile]

class PcapExample extends CodecSuite:

  import PcapCodec.*

  def bits = BitVector.fromMmap(new java.io.FileInputStream(new java.io.File("/path/to/pcap")).getChannel.nn)

  test("support reading an entire file and then decoding it all".ignore) {
    pcapFile.decode(bits)
  }

  test("support reading the file header and then decoding each record, combining results via a monoid".ignore) {
    val fileHeader = pcapHeader.decode(bits.take(28 * 8)).require.value
    implicit val ordering = fileHeader.ordering

    // Monoid that counts records
    val (_, recordCount) = pcapRecord.decodeAll(_ => 1)(0, _ + _)(bits.drop(28 * 8))

    // Monoid that accumulates records
    val (_, records) = pcapRecord.decodeAll(Vector(_))(Vector.empty, _ ++ _)(bits.drop(28 * 8))
  }

  // Alternatively, don't pre-load all bytes... read each record header individually and use included size field to read more bytes
  // See scodec-stream library at https://github.com/scodec/scodec-stream
