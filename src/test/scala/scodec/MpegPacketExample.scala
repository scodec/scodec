package scodec

import shapeless._


class MpegPacketExample extends CodecSuite {

  case class TransportStreamHeader(
    transportErrorIndicator: Boolean,
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
    payload: Option[BitVector]
  )
  implicit def mpegPacketIso = Iso.hlist(MpegPacket.apply _, MpegPacket.unapply _)

  object MpegCodecs {
    import Codecs._

    val uint13 = new IntCodec(13, signed = false)

    val transportStreamHeader: Codec[TransportStreamHeader] = {
      ("syncByte"                  | constant(0x47)          ) :~>:
      ("transportErrorIndicator"   | bool                    ) :~:
      ("payloadUnitStartIndicator" | bool                    ) :~:
      ("transportPriority"         | bool                    ) :~:
      ("pid"                       | uint13                  ) :~:
      ("scramblingControl"         | uint2                   ) :~:
      ("adaptationFieldControl"    | uint2                   ) :~:
      ("continuityCounter"         | uint4                   )
    }.as[TransportStreamHeader]

    val adaptationFieldFlags: Codec[AdaptationFieldFlags] = {
      ("discontinuity"             | bool                    ) :~:
      ("randomAccess"              | bool                    ) :~:
      ("priority"                  | bool                    ) :~:
      ("pcrFlag"                   | bool                    ) :~:
      ("opcrFlag"                  | bool                    ) :~:
      ("splicingPointFlag"         | bool                    ) :~:
      ("transportPrivateDataFlag"  | bool                    ) :~:
      ("adaptationFieldExtension"  | bool                    )
    }.as[AdaptationFieldFlags]

    val adaptationField: Codec[AdaptationField] = adaptationFieldFlags.flatPrepend { flags =>
      ("pcr"                       | conditional(flags.pcrFlag, bits(48))       ) :~:
      ("opcr"                      | conditional(flags.opcrFlag, bits(48))      ) :~:
      ("spliceCountdown"           | conditional(flags.splicingPointFlag, int8) )
    }.as[AdaptationField]

    val mpegPacket: Codec[MpegPacket] = transportStreamHeader.flatPrepend { hdr =>
      ("adaptation_field"          | conditional(hdr.adaptationFieldIncluded, adaptationField) ):~:
      ("payload"                   | conditional(hdr.payloadIncluded, bytes(184))              )
    }.as[MpegPacket]
  }



  test("roundtrip") {
    roundtrip(MpegCodecs.transportStreamHeader, TransportStreamHeader(false, true, false, 0, 0, 1, 15))

    val pkt = MpegPacket(TransportStreamHeader(false, true, false, 0, 0, 1, 15), None, Some(BitVector.low(184 * 8)))
    roundtrip(MpegCodecs.mpegPacket, pkt)
  }
}
