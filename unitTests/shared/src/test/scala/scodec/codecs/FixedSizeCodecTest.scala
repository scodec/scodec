package scodec
package codecs

import scodec.bits.BitVector
import scodec.bits.HexStringSyntax

class FixedSizeCodecTest extends CodecSuite {

  "the fixedSizeBits/fixedSizeBytes combinators" should {
    "roundtrip" in {
      roundtrip(fixedSizeBits(32, utf8), "test")
      roundtrip(fixedSizeBits(8, uint8), 12)
      roundtrip(fixedSizeBits(16, uint8), 12)
    }

     "pad appropriately" in {
      fixedSizeBits(16, uint8).encode(12).require shouldBe BitVector(hex"0c00")
    }


    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = utf8.encode("test").require
      fixedSizeBits(32, utf8).decode(encoded ++ BitVector.low(48)) shouldBe Attempt.successful(DecodeResult("test", BitVector.low(48)))
      fixedSizeBits(24, utf8).encode("test") shouldBe Attempt.failure(Err("[test] requires 32 bits but field is fixed size of 24 bits"))
    }
  }
}
