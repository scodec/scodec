package scodec
package codecs

import shapeless._
import nat._

class HListCodecTest extends CodecSuite {

  case class Foo(x: Int, y: Int, s: String)
  case class Bar(x: Int)
  case class Baz(a: Int, b: Int, c: Int, d: Int)

  "HList codec support" should {

    "support construction via :: operator" in {
      roundtripAll((uint8 :: uint8 :: ascii), Seq(1 :: 2 :: "test" :: HNil))
    }

    "support conversion HList codec to a case class codec via as method" in {
      roundtripAll((uint8 :: uint8 :: ascii).as[Foo], Seq(Foo(1, 2, "test")))
    }

    "support conversion of non-HList codec to a case class codec via as method" in {
      roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
    }

    "provide a flatPrepend method" in {
      uint8 flatPrepend { n => bits(n).hlist }
    }

    "provide a flatZipHList method" in {
      uint8 flatZipHList { n => bits(n) }
    }

    "provide ability to append via :+ operator" in {
      roundtrip(((uint8 :: uint8) :+ ascii).as[Foo], Foo(1, 2, "test"))
    }

    "provide ability to concatenate two HList codecs" in {
      roundtrip(((uint8 :: uint8) ::: (uint8 :: uint8)).as[Baz], Baz(1, 2, 3, 4))
    }

    "support HList equivalent of Codec#dropLeft" in {
      val codec = (uint8.unit(0) :~>: uint8.hlist).as[Bar]
      roundtrip(codec, Bar(1))
      codec.encodeValid(Bar(1)) should have size(16)
    }

    "support HList equivalent of Codec#dropLeft on a non-HList codec" in {
      uint8.unit(0) :~>: uint8
    }

    "support dropping all Unit values out of an HList codec" in {
      def ign(size: Int) = scodec.codecs.ignore(size)
      val codec = (uint8 :: ign(8) :: uint8 :: ign(8) :: ascii).dropUnits.as[Foo]
      roundtrip(codec, Foo(1, 2, "test"))
    }
  }
}
