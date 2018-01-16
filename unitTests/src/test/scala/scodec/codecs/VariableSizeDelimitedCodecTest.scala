package scodec
package codecs

import scodec.bits._

class VariableSizeDelimitedCodecTest extends CodecSuite {

  "the variableSizeDelimited codecs" should {
    "encode value followed by delimiter" in {
      variableSizeDelimited(constant(0), utf8, 8).encode("test") shouldBe Attempt.successful(BitVector('t', 'e', 's', 't', 0))
    }
    
    "decode value followed by delimiter" in {
      val buffer = BitVector('t', 'e', 's', 't', 0)
      println(s"VariableSizeDelimitedCodecTest ${buffer.size}")
      variableSizeDelimited(constant(0), ascii, 8).decode(buffer) shouldBe Attempt.successful(DecodeResult("test", BitVector.empty))
    }
    
    "decode value followed by delimiter with remainder" in {
      val buffer = BitVector('t', 'e', 's', 't', 0, 1, 2, 3)
      variableSizeDelimited(constant(0), ascii, 8).decode(buffer) shouldBe Attempt.successful(DecodeResult("test",BitVector(1, 2, 3)))
    }
  }
}
