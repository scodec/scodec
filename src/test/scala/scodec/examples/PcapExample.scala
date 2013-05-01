package scodec
package examples

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration._
import scalaz.std.AllInstances._
import scalaz.std.indexedSeq._
import shapeless._

/**
 * Processes libpcap files.
 *
 * @see http://wiki.wireshark.org/Development/LibpcapFileFormat
 */
object PcapCodec {
  import Codecs._

  sealed trait ByteOrdering
  case object BigEndian extends ByteOrdering
  case object LittleEndian extends ByteOrdering

  private val magicNumber = 0x000000a1b2c3d4L
  val byteOrdering = "magic_number" | Codec[ByteOrdering](
    (bo: ByteOrdering) => if (bo == BigEndian) uint32.encode(magicNumber) else uint32L.encode(magicNumber),
    (buf: BitVector) => uint32.decode(buf).map { case (rest, mn) =>
      (rest, if (mn == magicNumber) BigEndian else LittleEndian)
    }
  )

  def gint16(implicit ordering: ByteOrdering): Codec[Int] = if (ordering == BigEndian) int16 else int16L
  def guint16(implicit ordering: ByteOrdering): Codec[Int] = if (ordering == BigEndian) uint16 else uint16L
  def gint32(implicit ordering: ByteOrdering): Codec[Int] = if (ordering == BigEndian) int32 else int32L
  def guint32(implicit ordering: ByteOrdering): Codec[Long] = if (ordering == BigEndian) uint32 else uint32L

  case class PcapHeader(ordering: ByteOrdering, versionMajor: Int, versionMinor: Int, thiszone: Int, sigfigs: Long, snaplen: Long, network: Long)
  implicit val pcapHeaderIso = Iso.hlist(PcapHeader.apply _, PcapHeader.unapply _)

  implicit val pcapHeader = {
    ("magic_number"     | byteOrdering             ) >>:~ { implicit ordering =>
    ("version_major"    | guint16                  ) :~:
    ("version_minor"    | guint16                  ) :~:
    ("thiszone"         | gint32                   ) :~:
    ("sigfigs"          | guint32                  ) :~:
    ("snaplen"          | guint32                  ) :~:
    ("network"          | guint32                  )
  }}.as[PcapHeader]


  case class PcapRecordHeader(timestampSeconds: Long, timestampMicros: Long, includedLength: Long, originalLength: Long) {
    def timestamp: Double = timestampSeconds + (timestampMicros / (1.second.toMicros.toDouble))
  }
  implicit val pcapRecordHeaderIso = Iso.hlist(PcapRecordHeader.apply _, PcapRecordHeader.unapply _)

  implicit def pcapRecordHeader(implicit ordering: ByteOrdering) = {
    ("ts_sec"           | guint32                  ) :~:
    ("ts_usec"          | guint32                  ) :~:
    ("incl_len"         | guint32                  ) :~:
    ("orig_len"         | guint32                  )
  }.as[PcapRecordHeader]

  case class PcapRecord(header: PcapRecordHeader, data: BitVector)
  implicit val pcapRecordIso = Iso.hlist(PcapRecord.apply _, PcapRecord.unapply _)

  implicit def pcapRecord(implicit ordering: ByteOrdering) = {
    ("record_header"    | pcapRecordHeader         ) >>:~ { hdr =>
    ("record_data"      | bytes(hdr.includedLength.toInt) ).hlist
  }}.as[PcapRecord]

  case class PcapFile(header: PcapHeader, records: IndexedSeq[PcapRecord])
  implicit val pcapFileIso = Iso.hlist(PcapFile.apply _, PcapFile.unapply _)

  implicit val pcapFile = {
    pcapHeader >>:~ { hdr => repeated(pcapRecord(hdr.ordering)).hlist
  }}.as[PcapFile]
}


class PcapExample extends CodecSuite {

  import PcapCodec._

  test("pcap file reading") {
    pending

    // Option 1: Read the entire file and decode it all
    val bits = BitVector(com.google.common.io.Files.toByteArray(new java.io.File("/path/to/pcap")))
    Codec.decode[PcapFile](bits)

    // Option 2: read file header and then decode each record, combining results via a monoid
    val fileHeader = Codec.decode[PcapHeader](bits.take(28 * 8)) valueOr sys.error
    implicit val ordering = fileHeader.ordering

    // Monoid that counts records
    val (_, recordCount) = Codec.decodeAll[PcapRecord, Int](bits.drop(28 * 8)) { _ => 1 }

    // Monoid that accumulates records
    val (_, records) = Codec.decodeAll[PcapRecord, IndexedSeq[PcapRecord]](bits.drop(28 * 8)) { r => IndexedSeq(r) }

    // Alternatively, don't pre-load all bytes... read each record header individually and use included size field to read more bytes
    // TODO
  }
}
