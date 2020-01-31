package scodec
package codecs

import scodec.bits.BitVector

class OptionalCodecTest extends CodecSuite {

  "the optional combinator" should {

    "produce the target value on true" in {
      forAll { (n: Int) =>
        val codec = optional(provide(true), int32)
        shouldDecodeFullyTo(codec, BitVector.fromInt(n), Some(n))
      }
    }

    "produce none on false" in {
      forAll { (n: Int) =>
        val codec = optional(provide(false), int32)
        val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.fromInt(n))

        rest shouldBe BitVector.fromInt(n)
        b shouldBe None
      }
    }
  }
}
