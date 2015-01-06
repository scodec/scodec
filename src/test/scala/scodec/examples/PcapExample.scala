package scodec
package examples

import scala.concurrent.duration._
import shapeless._

import scodec.bits.BitVector

/**
 * Processes libpcap files.
 *
 * @see http://wiki.wireshark.org/Development/LibpcapFileFormat
 */
object PcapCodec {
  import scodec.codecs._

  sealed trait ByteOrdering
  case object BigEndian extends ByteOrdering
  case object LittleEndian extends ByteOrdering

  private val magicNumber = 0x000000a1b2c3d4L
  val byteOrdering = "magic_number" | Codec[ByteOrdering](
    (bo: ByteOrdering) => if (bo == BigEndian) uint32.encode(magicNumber) else uint32L.encode(magicNumber),
    (buf: BitVector) => uint32.decode(buf).map { _ mapValue { mn => if (mn == magicNumber) BigEndian else LittleEndian } }
  )

  def gint16(implicit ordering: ByteOrdering): Codec[Int] = if (ordering == BigEndian) int16 else int16L
  def guint16(implicit ordering: ByteOrdering): Codec[Int] = if (ordering == BigEndian) uint16 else uint16L
  def gint32(implicit ordering: ByteOrdering): Codec[Int] = if (ordering == BigEndian) int32 else int32L
  def guint32(implicit ordering: ByteOrdering): Codec[Long] = if (ordering == BigEndian) uint32 else uint32L

  case class PcapHeader(ordering: ByteOrdering, versionMajor: Int, versionMinor: Int, thiszone: Int, sigfigs: Long, snaplen: Long, network: Long)

  implicit val pcapHeader = {
    ("magic_number"     | byteOrdering             ) >>:~ { implicit ordering =>
    ("version_major"    | guint16                  ) ::
    ("version_minor"    | guint16                  ) ::
    ("thiszone"         | gint32                   ) ::
    ("sigfigs"          | guint32                  ) ::
    ("snaplen"          | guint32                  ) ::
    ("network"          | guint32                  )
  }}.as[PcapHeader]


  case class PcapRecordHeader(timestampSeconds: Long, timestampMicros: Long, includedLength: Long, originalLength: Long) {
    def timestamp: Double = timestampSeconds + (timestampMicros / (1.second.toMicros.toDouble))
  }

  implicit def pcapRecordHeader(implicit ordering: ByteOrdering) = {
    ("ts_sec"           | guint32                  ) ::
    ("ts_usec"          | guint32                  ) ::
    ("incl_len"         | guint32                  ) ::
    ("orig_len"         | guint32                  )
  }.as[PcapRecordHeader]

  case class PcapRecord(header: PcapRecordHeader, data: BitVector)

  implicit def pcapRecord(implicit ordering: ByteOrdering) = {
    ("record_header"    | pcapRecordHeader                   ) >>:~ { hdr =>
    ("record_data"      | bits(hdr.includedLength.toInt * 8) ).hlist
  }}.as[PcapRecord]

  case class PcapFile(header: PcapHeader, records: Vector[PcapRecord])

  implicit val pcapFile = {
    pcapHeader >>:~ { hdr => vector(pcapRecord(hdr.ordering)).hlist
  }}.as[PcapFile]
}


class PcapExample extends CodecSuite {

  import PcapCodec._

  "Pcap codecs" should {
    def bits = BitVector.fromMmap(new java.io.FileInputStream(new java.io.File("/path/to/pcap")).getChannel)
    "support reading an entire file and then decoding it all" in {
      pending
      Codec.decode[PcapFile](bits)
    }

    "support reading the file header and then decoding each record, combining results via a monoid" in {
      pending
      val fileHeader = Codec.decode[PcapHeader](bits.take(28 * 8)).require.value
      implicit val ordering = fileHeader.ordering

      // Monoid that counts records
      val (_, recordCount) = Codec.decodeAll[PcapRecord, Int](bits.drop(28 * 8))(0, _ + _) { _ => 1 }

      // Monoid that accumulates records
      val (_, records) = Codec.decodeAll[PcapRecord, Vector[PcapRecord]](bits.drop(28 * 8))(Vector.empty, _ ++ _) { r => Vector(r) }
    }

    // Alternatively, don't pre-load all bytes... read each record header individually and use included size field to read more bytes
    // See scodec-stream library at https://github.com/scodec/scodec-stream
  }
}
