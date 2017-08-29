package scodec
package codecs

import scodec.bits.BitVector

class VariableSizePrefixedCodecTest extends CodecSuite {

  "the variableSizePrefixedBits/variableSizePrefixedBytes codecs" should {
    "roundtrip" in {
      roundtripAll(variableSizePrefixedBits(uint8, int32, utf8), Seq(2 -> "", 3 -> "test"))
      roundtripAll(variableSizePrefixedBits(uint8, int32, utf8, 10), Seq(2 -> "", 3 -> "test"))
    }

    "encode size followed by value" in {
      variableSizePrefixedBytes(uint8, int32, utf8).encode(2 -> "test") shouldBe Attempt.successful(BitVector(4, 0, 0, 0, 2, 't', 'e', 's', 't'))
      variableSizePrefixedBits(uint8, int32, utf8).encode(2 -> "test") shouldBe Attempt.successful(BitVector(32, 0, 0, 0, 2, 't', 'e', 's', 't'))
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      variableSizePrefixedBytes(uint2, int32, utf8).encode(2 -> "too long") shouldBe Attempt.failure(Err("[too long] is too long to be encoded: 8 is greater than maximum value 3 for 2-bit unsigned integer"))
    }

    "support padding of size" in {
      variableSizePrefixedBits(uint8, int32, uint8, 0).encode(1 -> 0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x08))
      variableSizePrefixedBytes(uint8, int32, uint8, 0).encode(1 -> 0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x01))

      variableSizePrefixedBits(uint8, int32, uint8, 1).encode(1 -> 0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x09))
      variableSizePrefixedBytes(uint8, int32, uint8, 1).encode(1 -> 0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x02))
    }
  }
}

