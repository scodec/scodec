package scodec
package codecs

import scalaz.{ \/-, -\/ }
import scodec.bits.BitVector
import scodec.bits.HexStringSyntax

class PaddedFixedSizeCodecTest extends CodecSuite {
  val ones = constant(hex"ff")

  "the paddedfixedSizeBytes combinators" should {
    "roundtrip" in {
      roundtrip(paddedFixedSizeBytes(4, ascii, ones), "test")
      roundtrip(paddedFixedSizeBytes(1, uint8, ones), 12)
      roundtrip(paddedFixedSizeBytes(2, uint8, ones), 12)
    }

    "pad appropriately on encode" in {
      paddedFixedSizeBytes(1, uint8, ones).encodeValid(12) shouldBe BitVector(hex"0c")
      paddedFixedSizeBytes(2, uint8, ones).encodeValid(12) shouldBe BitVector(hex"0cff")
      paddedFixedSizeBytes(3, uint8, ones).encodeValid(12) shouldBe BitVector(hex"0cffff")
    }

    "failed padCodec encode only reported if used" in {
      paddedFixedSizeBytes(1, uint8, codecs.fail(Err("bad pad"))).encodeValid(12) shouldBe BitVector(hex"0c")
    }

    "report failed padCodec encode" in {
      paddedFixedSizeBytes(2, uint8, codecs.fail(Err("bad pad"))).encode(12) shouldBe -\/(Err("bad pad").pushContext("padding"))
    }

    "support pkcs5/pkcs7 style padding" in {
      val codec = paddedFixedSizeBytesDependent(4, uint8, n => constant(n / 8))
      codec.encodeValid(0) shouldBe hex"00030303".bits
      roundtripAll(codec, Seq(0, 1, 255))
      codec.decode(hex"00040404".bits) shouldBe -\/(Err("expected constant BitVector(8 bits, 0x03) but got BitVector(8 bits, 0x04)").pushContext("padding"))
    }
  }

  "the paddedfixedSizeBits combinators" should {
    "roundtrip" in {
      roundtrip(paddedFixedSizeBits(32, ascii, ones), "test")
      roundtrip(paddedFixedSizeBits(8, uint8, ones), 12)
      roundtrip(paddedFixedSizeBits(16, uint8, ones), 12)
    }

    "pad appropriately on encode" in {
      paddedFixedSizeBits(8, uint8, ones).encodeValid(12) shouldBe BitVector(hex"0c")
      paddedFixedSizeBits(16, uint8, ones).encodeValid(12) shouldBe BitVector(hex"0cff")
      paddedFixedSizeBits(24, uint8, ones).encodeValid(12) shouldBe BitVector(hex"0cffff")
    }

    "encode fails if padding does not exactly fill" in {
      paddedFixedSizeBits(13, uint8, ones).encode(12) shouldBe -\/(Err("padding overflows fixed size field of 13 bits").pushContext("padding"))
      paddedFixedSizeBits(27, uint8, ones).encode(12) shouldBe -\/(Err("padding overflows fixed size field of 27 bits").pushContext("padding"))
    }

    "decode validate remainder" in {
      paddedFixedSizeBits(8, uint8, ones).decodeValidValue(BitVector(hex"0c")) shouldBe 12
      paddedFixedSizeBits(24, uint8, ones).decodeValidValue(BitVector(hex"0cffff")) shouldBe 12
    }

    "decode  ignores remainder" in {
      paddedFixedSizeBits(24, uint8, constantLenient(hex"ff")).decodeValidValue(BitVector(hex"0c0000")) shouldBe 12
      paddedFixedSizeBits(11, uint8, constantLenient(hex"ff")).decodeValidValue(BitVector(hex"0c0000")) shouldBe 12
    }

    "decode fails if remaining bits do not match pad" in {
      paddedFixedSizeBits(8, uint8, ones).decodeValidValue(BitVector(hex"0c")) shouldBe 12
      paddedFixedSizeBits(9, uint8, ones).decode(BitVector(hex"0c00")) shouldBe -\/(Err.insufficientBits(8, 1).pushContext("padding"))
      paddedFixedSizeBits(16, uint8, ones).decode(BitVector(hex"0c00")) shouldBe -\/(Err("expected constant BitVector(8 bits, 0xff) but got BitVector(8 bits, 0x00)").pushContext("padding"))
      paddedFixedSizeBits(24, uint8, ones).decode(BitVector(hex"0cff11")) shouldBe -\/(Err("expected constant BitVector(8 bits, 0xff) but got BitVector(8 bits, 0x11)").pushContext("padding"))
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = ascii.encode("test").toOption.get
      paddedFixedSizeBits(32, ascii, ones).decode(encoded ++ BitVector.low(48)) shouldBe \/-((BitVector.low(48), "test"))
      paddedFixedSizeBits(24, ascii, ones).encode("test") shouldBe -\/(Err("[test] requires 32 bits but field is fixed size of 24 bits"))
    }

    "support pkcs5/pkcs7 style padding" in {
      paddedFixedSizeBitsDependent(32, uint8, n => constant(n)).encodeValid(0) shouldBe hex"00181818".bits
    }
  }
}
