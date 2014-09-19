package scodec
package codecs

import scalaz.{ \/-, -\/ }
import scodec.bits.BitVector
import scodec.bits.HexStringSyntax

class PaddedFixedSizeCodecTest extends CodecSuite {
  val ones = constant(hex"ff")

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
    
    "pad fails if padding does not exact fill" in {
      paddedFixedSizeBits(13, uint8, ones).encode(12) shouldBe -\/("[constant(BitVector(8 bits, 0xff))] overflows")
      paddedFixedSizeBits(27, uint8, ones).encode(12) shouldBe -\/("[constant(BitVector(8 bits, 0xff))] overflows")
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = ascii.encode("test").toOption.get
      paddedFixedSizeBits(32, ascii, ones).decode(encoded ++ BitVector.low(48)) shouldBe \/-((BitVector.low(48), "test"))
      paddedFixedSizeBits(24, ascii, ones).encode("test") shouldBe -\/("[test] requires 32 bits but field is fixed size of 24 bits")
    }
  }
}
