package scodec
package codecs

import scodec.bits.BitVector
import scodec.bits.HexStringSyntax

class PaddedFixedSizeCodecTest extends CodecSuite {
  val ones = constant(hex"ff")

  "the paddedfixedSizeBytes combinators" should {
    "roundtrip" in {
      roundtrip(paddedFixedSizeBytes(4, utf8, ones), "test")
      roundtrip(paddedFixedSizeBytes(1, uint8, ones), 12)
      roundtrip(paddedFixedSizeBytes(2, uint8, ones), 12)
    }

    "pad appropriately on encode" in {
      paddedFixedSizeBytes(1, uint8, ones).encode(12).require shouldBe BitVector(hex"0c")
      paddedFixedSizeBytes(2, uint8, ones).encode(12).require shouldBe BitVector(hex"0cff")
      paddedFixedSizeBytes(3, uint8, ones).encode(12).require shouldBe BitVector(hex"0cffff")
    }

    "failed padCodec encode only reported if used" in {
      paddedFixedSizeBytes(1, uint8, codecs.fail(Err("bad pad"))).encode(12).require shouldBe BitVector(hex"0c")
    }

    "report failed padCodec encode" in {
      paddedFixedSizeBytes(2, uint8, codecs.fail(Err("bad pad"))).encode(12) shouldBe Attempt.failure(Err("bad pad").pushContext("padding"))
    }

    "support pkcs5/pkcs7 style padding" in {
      val codec = paddedFixedSizeBytesDependent(4, uint8, n => constant(n / 8))
      codec.encode(0).require shouldBe hex"00030303".bits
      roundtripAll(codec, Seq(0, 1, 255))
      codec.decode(hex"00040404".bits) shouldBe Attempt.failure(Err("expected constant BitVector(8 bits, 0x03) but got BitVector(8 bits, 0x04)").pushContext("padding"))
    }
  }

  "the paddedfixedSizeBits combinators" should {
    "roundtrip" in {
      roundtrip(paddedFixedSizeBits(32, utf8, ones), "test")
      roundtrip(paddedFixedSizeBits(8, uint8, ones), 12)
      roundtrip(paddedFixedSizeBits(16, uint8, ones), 12)
    }

    "pad appropriately on encode" in {
      paddedFixedSizeBits(8, uint8, ones).encode(12).require shouldBe BitVector(hex"0c")
      paddedFixedSizeBits(16, uint8, ones).encode(12).require shouldBe BitVector(hex"0cff")
      paddedFixedSizeBits(24, uint8, ones).encode(12).require shouldBe BitVector(hex"0cffff")
    }

    "encode fails if padding does not exactly fill" in {
      paddedFixedSizeBits(13, uint8, ones).encode(12) shouldBe Attempt.failure(Err("padding overflows fixed size field of 13 bits").pushContext("padding"))
      paddedFixedSizeBits(27, uint8, ones).encode(12) shouldBe Attempt.failure(Err("padding overflows fixed size field of 27 bits").pushContext("padding"))
    }

    "decode validate remainder" in {
      paddedFixedSizeBits(8, uint8, ones).decode(BitVector(hex"0c")).require.value shouldBe 12
      paddedFixedSizeBits(24, uint8, ones).decode(BitVector(hex"0cffff")).require.value shouldBe 12
    }

    "decode ignores remainder" in {
      paddedFixedSizeBits(24, uint8, constantLenient(hex"ff")).decode(BitVector(hex"0c0000")).require.value shouldBe 12
      paddedFixedSizeBits(11, uint8, constantLenient(hex"ff")).decode(BitVector(hex"0c0000")).require.value shouldBe 12
    }

    "decode fails if remaining bits do not match pad" in {
      paddedFixedSizeBits(8, uint8, ones).decode(BitVector(hex"0c")).require.value shouldBe 12
      paddedFixedSizeBits(9, uint8, ones).decode(BitVector(hex"0c00")) shouldBe Attempt.failure(Err.insufficientBits(8, 1).pushContext("padding"))
      paddedFixedSizeBits(16, uint8, ones).decode(BitVector(hex"0c00")) shouldBe Attempt.failure(Err("expected constant BitVector(8 bits, 0xff) but got BitVector(8 bits, 0x00)").pushContext("padding"))
      paddedFixedSizeBits(24, uint8, ones).decode(BitVector(hex"0cff11")) shouldBe Attempt.failure(Err("expected constant BitVector(8 bits, 0xff) but got BitVector(8 bits, 0x11)").pushContext("padding"))
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = utf8.encode("test").require
      paddedFixedSizeBits(32, utf8, ones).decode(encoded ++ BitVector.low(48)) shouldBe Attempt.successful(DecodeResult("test", BitVector.low(48)))
      paddedFixedSizeBits(24, utf8, ones).encode("test") shouldBe Attempt.failure(Err("[test] requires 32 bits but field is fixed size of 24 bits"))
    }

    "support pkcs5/pkcs7 style padding" in {
      paddedFixedSizeBitsDependent(32, uint8, n => constant(n)).encode(0).require shouldBe hex"00181818".bits
    }
  }
}
