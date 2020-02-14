package scodec
package examples

import scala.concurrent.duration._

import scodec.bits.BitVector

/**
  * Processes libpcap files.
  *
  * @see http://wiki.wireshark.org/Development/LibpcapFileFormat
  */
object PcapCodec {
  import scodec.codecs._

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
  ) {
    def timestamp: Double = timestampSeconds + (timestampMicros / (1.second.toMicros.toDouble))
  }

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
}

class PcapExample extends CodecSuite {

  import PcapCodec._

  "Pcap codecs" should {
    def bits =
      BitVector.fromMmap(new java.io.FileInputStream(new java.io.File("/path/to/pcap")).getChannel)
    "support reading an entire file and then decoding it all" in {
      pending
      pcapFile.decode(bits)
      ()
    }

    "support reading the file header and then decoding each record, combining results via a monoid" in {
      pending
      val fileHeader = pcapHeader.decode(bits.take(28 * 8)).require.value
      implicit val ordering = fileHeader.ordering

      // Monoid that counts records
      val (_, recordCount) = pcapRecord.decodeAll(_ => 1)(0, _ + _)(bits.drop(28 * 8))

      // Monoid that accumulates records
      val (_, records) = pcapRecord.decodeAll(Vector(_))(Vector.empty, _ ++ _)(bits.drop(28 * 8))
    }

    // Alternatively, don't pre-load all bytes... read each record header individually and use included size field to read more bytes
    // See scodec-stream library at https://github.com/scodec/scodec-stream
  }
}
