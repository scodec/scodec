package scodec
package codecs

import scodec.bits.BitVector

class RecoverCodecTest extends CodecSuite {

  "the recover combinator" should {
    "always code a value for true value" in {
      forAll { (i: Int) =>
        val codec = recover(constant(BitVector fromInt i))
        val Attempt.Successful(DecodeResult(b2, rest)) = for {
          bits <- codec.encode(true)
          result <- codec.decode(bits)
        } yield result
        rest.isEmpty shouldBe true
        b2 shouldBe true
      }
    }

    "always code an empty vector for false value" in {
      forAll { (i: Int) =>
        val codec = recover(constant(BitVector fromInt i))
        val Attempt.Successful(DecodeResult(b2, rest)) = for {
          bits <- codec.encode(false)
          result <- codec.decode(bits)
        } yield result
        rest.isEmpty shouldBe true
        b2 shouldBe false
      }
    }

    "decode a false when target codec fails to decode" in {
      forAll { (i: Int) =>
        val codec = recover(constant(BitVector fromInt i))
        val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.empty)
        rest.isEmpty shouldBe true
        b shouldBe false
      }
    }

    "decode the wrong value as false and backtrack" in {
      forAll { (i1: Int, i2: Int) =>
        if (i1 != i2 && i1 >= 0 && i2 >= 0) {
          val codec = recover(constant(BitVector fromInt i1))
          val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector fromInt i2)
          rest shouldBe (BitVector fromInt i2)
          b shouldBe false
        }
      }
    }
  }

  "the lookahead combinator" should {
    "always code a value for true value" in {
      forAll { (i: Int) =>
        val codec = lookahead(constant(BitVector fromInt i))
        val Attempt.Successful((encoded, DecodeResult(b2, rest))) = for {
          bits <- codec.encode(true)
          result <- codec.decode(bits)
        } yield (bits, result)
        rest shouldBe encoded
        b2 shouldBe true
      }
    }

    "always code an empty vector for false value" in {
      forAll { (i: Int) =>
        val codec = lookahead(constant(BitVector fromInt i))
        val Attempt.Successful(DecodeResult(b2, rest)) = for {
          bits <- codec.encode(false)
          result <- codec.decode(bits)
        } yield result
        rest.isEmpty shouldBe true
        b2 shouldBe false
      }
    }

    "decode a false when target codec fails to decode" in {
      forAll { (i: Int) =>
        val codec = lookahead(constant(BitVector fromInt i))
        val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.empty)
        rest.isEmpty shouldBe true
        b shouldBe false
      }
    }

    "decode the wrong value as false and backtrack" in {
      forAll { (i1: Int, i2: Int) =>
        if (i1 != i2 && i1 >= 0 && i2 >= 0) {
          val codec = lookahead(constant(BitVector fromInt i1))
          val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector fromInt i2)
          rest shouldBe (BitVector fromInt i2)
          b shouldBe false
        }
      }
    }
  }
}
