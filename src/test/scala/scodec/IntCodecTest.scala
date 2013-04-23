package scodec

import scalaz.syntax.id._

import Codecs._


class IntCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(int32, Seq(0, 1, -1, Int.MaxValue, Int.MinValue))
    roundtripAll(int16, Seq(0, 1, -1, 32767, -32768))
    roundtripAll(uint16, Seq(0, 1, 65535))
    roundtripAll(uint4, 0 to 15)
  }

  test("range checking") {
    int16.encode(65536) shouldBe "65536 is greater than maximum value 32767 for 16-bit signed integer".left
    int16.encode(-32769) shouldBe "-32769 is less than minimum value -32768 for 16-bit signed integer".left
    uint16.encode(-1) shouldBe "-1 is less than minimum value 0 for 16-bit unsigned integer".left
  }

  test("decoding with too few bits") {
    int16.decode(BitVector.low(8)) shouldBe ("cannot acquire 16 bits from a vector that contains 8 bits".left)
  }
}
