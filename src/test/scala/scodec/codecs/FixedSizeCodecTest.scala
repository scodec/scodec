package scodec
package codecs

import scalaz.{\/-, -\/}
import scodec.bits.BitVector

class FixedSizeCodecTest extends CodecSuite {

  "the fixedSizeBits/fixedSizeBytes combinators" should {
    "roundtrip" in {
      roundtrip(fixedSizeBits(32, ascii), "test")
    }

    "fail encoding when value is too large to be encoded by size codec" in {
      val encoded = ascii.encode("test").toOption.get
      fixedSizeBits(32, ascii).decode(encoded ++ BitVector.low(48)) shouldBe \/-((BitVector.low(48), "test"))
      fixedSizeBits(24, ascii).encode("test") shouldBe -\/("[test] requires 32 bits but field is fixed size of 24 bits")
    }
  }
}
