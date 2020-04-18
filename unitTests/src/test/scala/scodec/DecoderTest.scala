package scodec

import scodec.bits._
import scodec.codecs._

class DecoderTest extends CodecSuite {

  test("as") {
    case class Foo(x: Int)
    val d: Decoder[Int] = uint8
    val e: Decoder[Foo] = d.as[Foo]
    assertEquals(e.decode(hex"ff".bits), Attempt.successful(DecodeResult(Foo(255), BitVector.empty)))
  }
}
