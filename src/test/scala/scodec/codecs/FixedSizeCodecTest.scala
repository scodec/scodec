package scodec
package codecs

import scalaz.{\/-, -\/}
import scodec.bits.BitVector
import scodec.bits.HexStringSyntax

class FixedSizeCodecTest extends CodecSuite {

  "the fixedSizeBits/fixedSizeBytes combinators" should {
    "roundtrip" in {
      roundtrip(fixedSizeBits(32, ascii), "test")
      roundtrip(fixedSizeBits(8, uint8), 12)
      roundtrip(fixedSizeBits(16, uint8), 12)
    }
    
     "pad appropriately" in {
      fixedSizeBits(16, uint8).encodeValid(12) shouldBe BitVector(hex"0c00")
    }


    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = ascii.encode("test").toOption.get
      fixedSizeBits(32, ascii).decode(encoded ++ BitVector.low(48)) shouldBe \/-((BitVector.low(48), "test"))
      fixedSizeBits(24, ascii).encode("test") shouldBe -\/("[test] requires 32 bits but field is fixed size of 24 bits")
    }
  }
}
