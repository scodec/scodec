package scodec
package codecs

import scodec.bits.{BitVector, HexStringSyntax}

class BytesStrictCodecTest extends CodecSuite {

  "the bytesStrict codec" should {

    "roundtrip when input size is correct" in { roundtrip(bytesStrict(8), hex"0102030405060708") }

    "return an error when encoding with less bytes than expected" in {
      bytesStrict(8).encode(hex"0102030405") shouldBe Attempt.failure(
        Err(
          "[BitVector(40 bits, 0x0102030405)] requires 40 bits but field is fixed size of exactly 64 bits"
        )
      )
    }

    "return an error when encoding with more bytes than expected" in {
      bytesStrict(8).encode(hex"010203040506070809") shouldBe Attempt.failure(
        Err(
          "[BitVector(72 bits, 0x010203040506070809)] requires 72 bits but field is fixed size of exactly 64 bits"
        )
      )
    }

    "return an error when decoding with less bytes than expected" in {
      bytesStrict(8).decode(BitVector(hex"0102030405")) shouldBe Attempt.failure(
        Err("expected exactly 64 bits but got 40 bits")
      )
    }

    "return an error when decoding with more bytes than expected" in {
      bytesStrict(8).decode(BitVector(hex"010203040506070809")) shouldBe Attempt.failure(
        Err("expected exactly 64 bits but got 72 bits")
      )
    }
  }
}
