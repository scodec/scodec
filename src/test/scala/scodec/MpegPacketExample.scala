package scodec

import org.scalatest._


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

  object MpegCodecs {
    import Codecs._

    val uint13 = new IntCodec(13, signed = false)

    val mpegPacketHeader: Codec[MpegPacketHeader] = {
      ("syncByte"                  :: constant(0x47)  ) ~> (
      ("transportErrorIndicator"   :: bool            ) ~
      ("payloadUnitStartIndicator" :: bool            ) ~
      ("transportPriority"         :: bool            ) ~
      ("pid"                       :: uint13          ) ~
      ("scramblingControl"         :: uint2           ) ~
      ("adaptationFieldControl"    :: uint2           ) ~
      ("continuityCounter"         :: uint4           ))
    }.xmap(
      { case tsi ~ pusi ~ tprio ~ pid ~ scr ~ afc ~ cc => MpegPacketHeader(tsi, pusi, tprio, pid, scr, afc, cc) },
      { hdr => hdr.transportErrorIndicator ~ hdr.payloadUnitStartIndicator ~ hdr.transportPriority ~ hdr.pid ~ hdr.scramblingControl ~ hdr.adaptationFieldControl ~ hdr.continuityCounter }
    )
  }
}
