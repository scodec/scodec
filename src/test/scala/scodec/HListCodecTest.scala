package scodec

import shapeless._

import Codecs._


class HListCodecTest extends CodecSuite {

  case class Foo(x: Int, y: Int, s: String)
  implicit def fooIso = Iso.hlist(Foo.apply _, Foo.unapply _)

  test("roundtrip") {
    roundtripAll((uint8 :: uint8 :: ascii), Seq(1 :: 2 :: "test" :: HNil))
    roundtripAll((uint8 :: uint8 :: ascii).as[Foo], Seq(Foo(1, 2, "test")))
  }

  test("xmap non-hlist codec to case class") {
    case class Bar(x: Int)
    implicit val barIso = Iso.hlist(Bar.apply _, Bar.unapply _)
    roundtripAll(uint8.hlist.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }

  test("flatPrepend") {
    uint8 flatPrepend { n => bits(n).hlist }
  }

  test("flatZipHList") {
    uint8 flatZipHList { n => bits(n) }
  }

  test("append via :+") {
    roundtrip(((uint8 :: uint8) :+ ascii).as[Foo], Foo(1, 2, "test"))
  }

  test("concat via :::") {
    case class Bar(a: Int, b: Int, c: Int, d: Int)
    implicit val barIso = Iso.hlist(Bar.apply _, Bar.unapply _)
    roundtrip(((uint8 :: uint8) ::: (uint8 :: uint8)).as[Bar], Bar(1, 2, 3, 4))
  }
}
