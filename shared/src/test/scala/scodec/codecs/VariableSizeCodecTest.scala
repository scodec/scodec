package scodec
package codecs

import scodec.bits._

class VariableSizeCodecTest extends CodecSuite {

  "the variableSizeBits/variableSizeBytes codecs" should {
    "roundtrip" in {
      roundtripAll(variableSizeBits(uint8, utf8), Seq("", "test"))
      roundtripAll(variableSizeBits(uint8, utf8, 10), Seq("", "test"))
    }

    "encode size followed by value" in {
      variableSizeBytes(uint8, utf8).encode("test") shouldBe Attempt.successful(BitVector(4, 't', 'e', 's', 't'))
      variableSizeBits(uint8, utf8).encode("test") shouldBe Attempt.successful(BitVector(32, 't', 'e', 's', 't'))
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      variableSizeBytes(uint2, utf8).encode("too long") shouldBe Attempt.failure(Err.General("failed to encode size of [too long]: 8 is greater than maximum value 3 for 2-bit unsigned integer", List("size")))
    }

    "support padding of size" in {
      variableSizeBits(uint8, uint8, 0).encode(0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x08))
      variableSizeBytes(uint8, uint8, 0).encode(0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x01))

      variableSizeBits(uint8, uint8, 1).encode(0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x09))
      variableSizeBytes(uint8, uint8, 1).encode(0).map { _.take(8) } shouldBe Attempt.successful(BitVector(0x02))
    }
  }

  "the variableSizeBytes codec" should {
    "fail encoding if the wrapped codec returns a bit vector with length not divisible by 8" in {
      variableSizeBytes(uint8, bool).encode(true) shouldBe Attempt.failure(Err.General("failed to encode size of [true]: 1 is not evenly divisible by 8", List("size")))
      variableSizeBytes(uint8, byteAligned(bool)).encode(true) shouldBe Attempt.successful(bin"00000001 10000000")
    }
  }
}
