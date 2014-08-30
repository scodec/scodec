package scodec
package codecs

import scalaz.std.AllInstances._

import shapeless._
import nat._

class HListCodecTest extends CodecSuite {

  case class Foo(x: Int, y: Int, s: String)
  case class Bar(x: Int)

  test("roundtrip") {
    roundtripAll((uint8 :: uint8 :: ascii), Seq(1 :: 2 :: "test" :: HNil))
    roundtripAll((uint8 :: uint8 :: ascii).as[Foo], Seq(Foo(1, 2, "test")))
  }

  test("xmap non-hlist codec to case class") {
    roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
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

  case class Baz(a: Int, b: Int, c: Int, d: Int)
  test("concat via :::") {
    roundtrip(((uint8 :: uint8) ::: (uint8 :: uint8)).as[Baz], Baz(1, 2, 3, 4))
  }

  test("dropLeft") {
    val codec = (uint8 :~>: uint8.hlist).as[Bar]
    roundtrip(codec, Bar(1))
    codec.encodeValid(Bar(1)) should have size(16)
  }

  test("dropLeft on non-hlist codec") {
    uint8 :~>: uint8
  }

  test("dropUnits") {
    def ign(size: Int) = scodec.codecs.ignore(size)
    val codec = (uint8 :: ign(8) :: uint8 :: ign(8) :: ascii).dropUnits.as[Foo]
    roundtrip(codec, Foo(1, 2, "test"))
  }
}
