package scodec

import scodec.bits._
import scodec.codecs._

class DecoderTest extends CodecSuite {

  "decoders" should {
    "support `as` transforms" in {
      case class Foo(x: Int)
      val d: Decoder[Int] = uint8
      val e: Decoder[Foo] = d.as[Foo]
      e.decode(hex"ff".bits) shouldBe Attempt.successful(DecodeResult(Foo(255), BitVector.empty))
    }
  }
}
