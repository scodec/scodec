package scodec
package codecs

import org.scalacheck.Gen

class VarIntCodecTest extends CodecSuite {
  def check(low: Int, high: Int, size: Int)(codec: Codec[Int]) = {
    forAll(Gen.choose(low, high)) { n =>
      codec.encode(n).map(_.bytes.size) shouldBe Attempt.successful(size)
    }
  }

  "the vint codec" should {
    "roundtrip" in forAll(Gen.choose(Int.MinValue, Int.MaxValue))(roundtrip(vint, _))

    "use 1 byte for ints <= 7 bits" in check(0, 127, 1)(vint)
    "use 2 bytes for ints <= 14 bits" in check(128, 16383, 2)(vint)
    "use 3 bytes for ints <= 21 bits" in check(16384, 2097151, 3)(vint)
    "use 4 bytes for ints <= 28 bits" in check(2097152, 268435455, 4)(vint)
    "use 5 bytes for ints <= 32 bits" in check(268435456, Int.MaxValue, 5)(vint)
    "use 5 bytes for negative ints" in check(Int.MinValue, -1, 5)(vint)
  }

  "the vintL codec" should {
    "roundtrip" in forAll(Gen.choose(0, Int.MaxValue))(roundtrip(vintL, _))

    "use 1 byte for ints <= 7 bits" in check(0, 127, 1)(vintL)
    "use 2 bytes for ints <= 14 bits" in check(128, 16383, 2)(vintL)
    "use 3 bytes for ints <= 21 bits" in check(16384, 2097151, 3)(vintL)
    "use 4 bytes for ints <= 28 bits" in check(2097152, 268435455, 4)(vintL)
    "use 5 bytes for ints <= 32 bits" in check(268435456, Int.MaxValue, 5)(vintL)
    "use 5 bytes for negative ints" in check(Int.MinValue, -1, 5)(vintL)
  }
}
