package scodec
package codecs

import scodec.bits.{BitVector, HexStringSyntax}

class BytesCodecTest extends CodecSuite {

  "the bytes codec" should {

    "roundtrip when input size match" in { roundtrip(bytes(8), hex"0102030405060708") }

    "pad with zero when encoding with less bytes than expected" in {
      bytes(8).encode(hex"0101010101").require shouldBe BitVector(hex"0101010101000000")
    }

    "return an error when encoding with more bytes than expected" in {
      bytes(8).encode(hex"010203040506070809") shouldBe Attempt.failure(Err("[BitVector(72 bits, 0x010203040506070809)] requires 72 bits but field is fixed size of 64 bits"))
    }
  }
}
