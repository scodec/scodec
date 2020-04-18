package scodec

import scodec.bits._
import scodec.codecs._

class EncoderTest extends CodecSuite {

  test("as") {
    case class Foo(x: Int)
    val a: Encoder[Int] = uint8
    val b: Encoder[Foo] = a.as[Foo]
    assertEquals(b.encode(Foo(255)), Attempt.successful(hex"ff".bits))
  }
}
