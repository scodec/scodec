package scodec
package examples

import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

// Define MPEG codecs
object MpegCodecs {

  // Define case classes that describe MPEG packets and define an HList iso for each
  //
  // Note: This encoding is a simplified version of what's required to parse real
  // MPEG packets. For a production quality codec, see the
  // [Packet](https://github.com/scodec/scodec-protocols/blob/series/1.0.x/src/main/scala/scodec/protocols/mpeg/transport/Packet.scala)
  // and associated data types from scodec-protocols.

  case class TransportStreamHeader(
      transportStringIndicator: Boolean,
      payloadUnitStartIndicator: Boolean,
      transportPriority: Boolean,
      pid: Int,
      scramblingControl: Int,
      adaptationFieldControl: Int,
      continuityCounter: Int
  ) {
    def adaptationFieldIncluded: Boolean = adaptationFieldControl >= 2
    def payloadIncluded: Boolean = adaptationFieldControl == 1 || adaptationFieldControl == 3
  }

  case class AdaptationFieldFlags(
      discontinuity: Boolean,
      randomAccess: Boolean,
      priority: Boolean,
      pcrFlag: Boolean,
      opcrFlag: Boolean,
      splicingPointFlag: Boolean,
      transportPrivateDataFlag: Boolean,
      adaptationFieldExtension: Boolean
  )

  case class AdaptationField(
      flags: AdaptationFieldFlags,
      pcr: Option[BitVector],
      opcr: Option[BitVector],
      spliceCountdown: Option[Int]
  )

  case class MpegPacket(
      header: TransportStreamHeader,
      adaptationField: Option[AdaptationField],
      payload: Option[ByteVector]
  )

  implicit val transportStreamHeader: Codec[TransportStreamHeader] = fixedSizeBytes(
    4, {
      ("syncByte" | constant(0x47)) :~>:
        ("transportStringIndicator" | bool) ::
        ("payloadUnitStartIndicator" | bool) ::
        ("transportPriority" | bool) ::
        ("pid" | uint(13)) ::
        ("scramblingControl" | uint2) ::
        ("adaptationFieldControl" | uint2) ::
        ("continuityCounter" | uint4)
    }
  ).as[TransportStreamHeader]

  implicit val adaptationFieldFlags: Codec[AdaptationFieldFlags] = fixedSizeBytes(
    1, {
      ("discontinuity" | bool) ::
        ("randomAccess" | bool) ::
        ("priority" | bool) ::
        ("pcrFlag" | bool) ::
        ("opcrFlag" | bool) ::
        ("splicingPointFlag" | bool) ::
        ("transportPrivateDataFlag" | bool) ::
        ("adaptationFieldExtension" | bool)
    }
  ).as[AdaptationFieldFlags]

  implicit val adaptationField: Codec[AdaptationField] = {
    ("adaptation_flags" | adaptationFieldFlags) >>:~ { flags =>
      ("pcr" | conditional(flags.pcrFlag, bits(48))) ::
        ("opcr" | conditional(flags.opcrFlag, bits(48))) ::
        ("spliceCountdown" | conditional(flags.splicingPointFlag, int8))
    }
  }.as[AdaptationField]

  implicit val mpegPacket: Codec[MpegPacket] = {
    ("header" | transportStreamHeader) >>:~ { hdr =>
      ("adaptation_field" | conditional(hdr.adaptationFieldIncluded, adaptationField)) ::
        ("payload" | conditional(hdr.payloadIncluded, bytes(184)))
    }
  }.as[MpegPacket]
}

class MpegPacketExample extends CodecSuite {

  import MpegCodecs._

  "MpegPacket codec" should {
    "roundtrip" in {
      val pkt = MpegPacket(
        TransportStreamHeader(false, true, false, 0, 0, 1, 15),
        None,
        Some(BitVector.low(184 * 8).toByteVector)
      )
      val encoded = Codec.encode(pkt).require
      val decoded = Codec.decode[MpegPacket](encoded).require.value
      decoded shouldBe pkt
    }
  }
}
