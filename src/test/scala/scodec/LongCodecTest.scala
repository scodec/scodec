package scodec

import scalaz.syntax.id._

import Codecs._

class LongCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(int64, Seq(0, 1, -1, Long.MaxValue, Long.MinValue))
    roundtripAll(uint32, Seq(0, 1, (1L << 32) - 1))
    roundtripAll(int64L, Seq(0, 1, -1, Long.MaxValue, Long.MinValue))
    roundtripAll(long(48), Seq(0L, 1L, -1L))
  }

  test("range checking") {
    uint32.encode(-1) shouldBe "-1 is less than minimum value 0 for 32-bit unsigned integer".left
  }

  test("decoding with too few bits") {
    uint32.decode(BitVector.low(8)) shouldBe ("cannot acquire 32 bits from a vector that contains 8 bits".left)
  }

  test("endianess") {
    Seq(Int.MaxValue.toLong, Long.MaxValue) foreach { value =>
      val bigEndian = int64.encode(value).toOption.get.toByteVector
      val littleEndian = int64L.encode(value).toOption.get.toByteVector
      littleEndian shouldBe bigEndian.reverse
    }
  }
}
