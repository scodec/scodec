package scodec
package codecs

import scodec.bits.BitVector
import scodec.bits.HexStringSyntax

class LimitedSizeCodecTest extends CodecSuite {

  "the limitedSizeBits/limitedSizeBytes combinators" should {
    "roundtrip" in {
      roundtrip(limitedSizeBits(32, utf8), "test")
      roundtrip(limitedSizeBits(8, uint8), 12)
      roundtrip(limitedSizeBits(16, uint8), 12)
    }

    "not pad" in {
      limitedSizeBits(16, uint8).encode(12).require shouldBe BitVector(hex"0c")
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = utf8.encode("test").require
      limitedSizeBits(32, utf8).decode(encoded ++ BitVector.low(48)) shouldBe Attempt.successful(
        DecodeResult("test", BitVector.low(48))
      )
      limitedSizeBits(24, utf8).encode("test") shouldBe Attempt.failure(
        Err("[test] requires 32 bits but field is limited to 24 bits")
      )
    }
  }
}
