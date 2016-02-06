package scodec

import scodec.bits._
import scodec.codecs._

class EncoderTest extends CodecSuite {

  "encoders" should {
    "support `as` transforms" in {
      case class Foo(x: Int)
      val a: Encoder[Int] = uint8
      val b: Encoder[Foo] = a.as[Foo]
      b.encode(Foo(255)) shouldBe Attempt.successful(hex"ff".bits)
    }
  }
}

