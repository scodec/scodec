package scodec

import org.scalatest._
import shapeless._


class MpegPacketExample {

  case class MpegPacketHeader(
    transportErrorIndicator: Boolean,
    payloadUnitStartIndicator: Boolean,
    transportPriority: Boolean,
    pid: Int,
    scramblingControl: Int,
    adaptationFieldControl: Int,
    continuityCounter: Int
  )
  implicit def mpegPacketHeaderIso = Iso.hlist(MpegPacketHeader.apply _, MpegPacketHeader.unapply _)

  object MpegCodecs {
    import Codecs._

    val uint13 = new IntCodec(13, signed = false)

    val mpegPacketHeader: Codec[MpegPacketHeader] = {
      ("syncByte"                  | constant(0x47) ) :~>:
      ("transportErrorIndicator"   | bool           ) :~:
      ("payloadUnitStartIndicator" | bool           ) :~:
      ("transportPriority"         | bool           ) :~:
      ("pid"                       | uint13         ) :~:
      ("scramblingControl"         | uint2          ) :~:
      ("adaptationFieldControl"    | uint2          ) :~:
      ("continuityCounter"         | uint4          )
    }.as[MpegPacketHeader]
  }
}
