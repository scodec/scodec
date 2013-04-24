package scodec

import shapeless._

import Codecs._


class CodecTest extends CodecSuite {

  test("flatZip") {
    val codec = uint8 flatZip { n => fixedSizeBits(n, ascii) }
    roundtripAll(codec, Seq((0, ""), (8, "a"), (32, "test")))
  }

  test("as on single codec") {
    case class Bar(x: Int)
    implicit val barIntIso = isoFromFunctions[Bar, Int](_.x, Bar.apply)
    roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }
}
