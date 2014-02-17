package scodec

import shapeless._

import scodec.codecs._

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

  /** Builds an `Iso[A, B]` from two functions. */
  final def isoFromFunctions[A, B](to: A => B, from: B => A): Iso[A, B] = {
    val toFn = to
    val fromFn = from
    new Iso[A, B] {
      def to(a: A) = toFn(a)
      def from(b: B) = fromFn(b)
    }
  }
}
