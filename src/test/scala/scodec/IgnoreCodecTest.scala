package scodec

import scalaz.\/-

import Codecs._


class IgnoreCodecTest extends CodecSuite {

  test("roundtrip") {
    val uint2 = new IntCodec(2, signed = false)
    val codec = Codecs.ignore(2) ~> uint4 <~ Codecs.ignore(2)
    codec.decode(BitVector(0xff)) shouldBe \/-((BitVector.empty, 15))
    codec.encode(15) shouldBe \/-(BitVector(0x3c))
  }
}
