package scodec
package examples

import scodec.bits.ByteVector

class UdpDatagramExample extends CodecSuite {

  case class Datagram(sourcePort: Int, destinationPort: Int, checksum: Int, data: ByteVector)

  object Datagram {
    import scodec.codecs._

    given Codec[Datagram] = {
      ("source_port" | uint16) ::
        ("dest_port" | uint16) ::
        variableSizeBytes(
          uint16,
          ("checksum" | uint16) ::
            ("data" | bytes)
        )
    }.as[Datagram]
  }

  "Datagram codec" should {
    "roundtrip" in {
      roundtrip(Datagram(1234, 2345, 0, ByteVector.fill(0xff)(100)))
    }
  }
}
