package scodec
package examples

import scalaz.\/-
import scalaz.std.anyVal.unitInstance
import shapeless._

import scodec.bits.{ BitVector, ByteVector }
import scodec.codecs._

// Define MPEG codecs
object MpegCodecs {

  // Define case classes that describe MPEG packets and define an HList iso for each

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
  implicit def tsHeaderIso = Iso.hlist(TransportStreamHeader.apply _, TransportStreamHeader.unapply _)

  case class AdaptationFieldFlags(
    discontinuity: Boolean,
    randomAccess: Boolean,
    priority: Boolean,
    pcrFlag: Boolean,
    opcrFlag: Boolean,
    splicingPointFlag: Boolean,
    transportPrivateDataFlag: Boolean,
    adaptationFieldExtension: Boolean)
  implicit def mpegAdaptationFieldFlagsIso = Iso.hlist(AdaptationFieldFlags.apply _, AdaptationFieldFlags.unapply _)

  case class AdaptationField(
    flags: AdaptationFieldFlags,
    pcr: Option[BitVector],
    opcr: Option[BitVector],
    spliceCountdown: Option[Int]
  )
  implicit def mpegAdaptationFieldIso = Iso.hlist(AdaptationField.apply _, AdaptationField.unapply _)

  case class MpegPacket(
    header: TransportStreamHeader,
    adaptationField: Option[AdaptationField],
    payload: Option[ByteVector]
  )
  implicit def mpegPacketIso = Iso.hlist(MpegPacket.apply _, MpegPacket.unapply _)

  implicit val transportStreamHeader: Codec[TransportStreamHeader] = {
    ("syncByte"                  | constant(0x47)          ) :~>:
    ("transportStringIndicator"   | bool                    ) ::
    ("payloadUnitStartIndicator" | bool                    ) ::
    ("transportPriority"         | bool                    ) ::
    ("pid"                       | uint(13)                ) ::
    ("scramblingControl"         | uint2                   ) ::
    ("adaptationFieldControl"    | uint2                   ) ::
    ("continuityCounter"         | uint4                   )
  }.as[TransportStreamHeader]

  implicit val adaptationFieldFlags: Codec[AdaptationFieldFlags] = {
    ("discontinuity"             | bool                    ) ::
    ("randomAccess"              | bool                    ) ::
    ("priority"                  | bool                    ) ::
    ("pcrFlag"                   | bool                    ) ::
    ("opcrFlag"                  | bool                    ) ::
    ("splicingPointFlag"         | bool                    ) ::
    ("transportPrivateDataFlag"  | bool                    ) ::
    ("adaptationFieldExtension"  | bool                    )
  }.as[AdaptationFieldFlags]

  implicit val adaptationField: Codec[AdaptationField] = {
    ("adaptation_flags"          | adaptationFieldFlags                       ) >>:~ { flags =>
    ("pcr"                       | conditional(flags.pcrFlag, bits(48))       ) ::
    ("opcr"                      | conditional(flags.opcrFlag, bits(48))      ) ::
    ("spliceCountdown"           | conditional(flags.splicingPointFlag, int8) )
  }}.as[AdaptationField]

  implicit val mpegPacket: Codec[MpegPacket] = {
    ("header"                    | transportStreamHeader                                     ) >>:~ { hdr =>
    ("adaptation_field"          | conditional(hdr.adaptationFieldIncluded, adaptationField) ) ::
    ("payload"                   | conditional(hdr.payloadIncluded, bytes(184))              )
  }}.as[MpegPacket]
}


class MpegPacketExample extends CodecSuite {

  import MpegCodecs._

  test("manually roundtripping a packet") {
    val pkt = MpegPacket(TransportStreamHeader(false, true, false, 0, 0, 1, 15), None, Some(BitVector.low(184 * 8).toByteVector))
    val encoded = Codec.encode(pkt)
    val decoded = Codec.decode[MpegPacket](encoded.toOption.get)
    decoded shouldBe \/-(pkt)
  }
}
