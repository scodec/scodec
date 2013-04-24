package scodec

import shapeless._

import Codecs._


class HListCodecTest extends CodecSuite {

  case class Foo(x: Int, y: Int, s: String)
  implicit def fooIso = Iso.hlist(Foo.apply _, Foo.unapply _)

  test("roundtrip") {
    roundtripAll((uint8 :~: uint8 :~: ascii), Seq(1 :: 2 :: "test" :: HNil))
    roundtripAll((uint8 :~: uint8 :~: ascii).as[Foo], Seq(Foo(1, 2, "test")))
  }

}
