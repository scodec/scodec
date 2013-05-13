package scodec
package examples

import shapeless._

class UdpDatagramExample extends CodecSuite {

  case class Datagram(
    sourcePort: Int,
    destinationPort: Int,
    length: Int,
    checksum: Int,
    data: BitVector)
  implicit val datagramIso = Iso.hlist(Datagram.apply _, Datagram.unapply _)

  object UdpCodec {
    import Codecs._

    implicit val datagram = {
      ("source_port" | uint16        ) ::
      ("dest_port"   | uint16        ) ::
      ("length"      | uint16        ).>>:~ { length =>
      ("checksum"    | uint16        ) ::
      ("data"        | bytes(length) )
    }}.as[Datagram]
  }

  import UdpCodec._

  test("roundtrip") {
    roundtrip(Datagram(1234, 2345, 100, 0, BitVector.high(100 * 8)))
  }
}
