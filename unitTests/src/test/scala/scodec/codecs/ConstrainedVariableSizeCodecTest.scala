package scodec
package codecs

import scodec.bits._

class ConstrainedVariableSizeCodecTest extends CodecSuite {

  "the constrainedVariableSizeBytes codecs" should {
    "roundtrip" in {

      roundtripAll(constrainedVariableSizeBytes(uint8, utf8, 8), "" :: "a" :: "ab" :: "abc" :: "abcd" :: "áêçã" :: Nil )
      roundtripAll(constrainedVariableSizeBytes(uint16, utf8, 8), "" :: "a" :: "ab" :: "abc" :: "abcd" :: "áêçã" :: Nil )

      roundtripAll(constrainedVariableSizeBytes(uint8, utf8, 3, 8), "abc" :: "abcd" :: "áêçã" :: Nil)
      roundtripAll(constrainedVariableSizeBytes(uint16, utf8, 3, 8), "abc" :: "abcd" :: "áêçã" :: Nil)
    }

    "forbid encoding under lower boundaries" in {
      constrainedVariableSizeBytes(uint8, utf8, 3, 8).encode("") shouldBe Attempt.Failure(Err("Size out of bounds: 24 <= 0 <= 64 is not true"))
      constrainedVariableSizeBytes(uint16, utf8, 3, 8).encode("") shouldBe Attempt.Failure(Err("Size out of bounds: 24 <= 0 <= 64 is not true"))
    }

    "forbid encoding over upper boundaries" in {
      constrainedVariableSizeBytes(uint8, utf8, 3).encode("çãéá") shouldBe Attempt.Failure(Err("Size out of bounds: 0 <= 64 <= 24 is not true"))
      constrainedVariableSizeBytes(uint16, utf8, 3).encode("çãéá") shouldBe Attempt.Failure(Err("Size out of bounds: 0 <= 64 <= 24 is not true"))
    }

    "forbid decoding under lower boundaries" in {
      constrainedVariableSizeBytes(uint8, utf8, 3, 8).decode(hex"08 30".bits) shouldBe Attempt.Failure(Err("Size out of bounds: 24 <= 8 <= 64 is not true"))
      constrainedVariableSizeBytes(uint16, utf8, 3, 8).decode(hex"00 08 30".bits) shouldBe Attempt.Failure(Err("Size out of bounds: 24 <= 8 <= 64 is not true"))
    }

    "forbid decoding over upper boundaries" in {
      constrainedVariableSizeBytes(uint8, utf8, 2).decode(hex"18 30 30 30".bits) shouldBe Attempt.Failure(Err("Size out of bounds: 0 <= 24 <= 16 is not true"))
      constrainedVariableSizeBytes(uint16, utf8, 2).decode(hex"00 18 30 30 30".bits) shouldBe Attempt.Failure(Err("Size out of bounds: 0 <= 24 <= 16 is not true"))
    }
  }
}
