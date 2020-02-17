package scodec
package codecs

import scodec.bits._

class TupleCodecTest extends CodecSuite {

  case class Foo(x: Int, y: Int, s: String)
  case class Bar(x: Int)
  case class Baz(a: Int, b: Int, c: Int, d: Int)
  case class Flags(x: Boolean, y: Boolean, z: Boolean)

  "tuple codec support" should {

    "support construction via :: operator" in {
      roundtrip((uint8 :: uint8 :: utf8), (1, 2, "test"))
    }

    "support conversion of tuple codec to a case class codec via as method" in {
      roundtrip((uint8 :: uint8 :: utf8).as[Foo], Foo(1, 2, "test"))
    }

    "support conversion of non-tuple codec to a case class codec via as method" in {
      roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
    }

    "support converting an tuple of codecs" in {
      val a: Codec[(Int, Long, Boolean)] = Codec.fromTuple(uint8, int64, bool)
      roundtrip(a, (1, 2L, true))
    }

    "provide a flatPrepend method" in {
      uint8.flatPrepend { n =>
        bits(n.toLong).tuple
      }
      ()
    }

    "provide ability to append via :+ operator" in {
      roundtrip(((uint8 :: uint8) :+ utf8).as[Foo], Foo(1, 2, "test"))
    }

    "provide ability to concatenate two tuple codecs" in {
      roundtrip(((uint8 :: uint8) ++ (uint8 :: uint8)).as[Baz], Baz(1, 2, 3, 4))
    }

    "support dropping all Unit values out of a tuple codec" in {
      def ign(size: Int) = scodec.codecs.ignore(size.toLong)
      val codec = (uint8 :: ign(8) :: uint8 :: ign(8) :: utf8).dropUnits.as[Foo]
      roundtrip(codec, Foo(1, 2, "test"))
    }

    "flatConcat" in {
      val c = (uint8 :: uint8).flatConcat(_ => uint8 :: uint8)
      roundtrip(c, (1, 2, 3, 4))
    }

    "flatAppend" in {
      val c = (uint8 :: uint8).flatAppend(_ => uint8)
      roundtrip(c, (1, 2, 3))
    }

    "support removing an element of an tuple codec by type" in {
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
      val valuesWithFlags = flagsCodec.flatPrepend { flgs =>
        conditional(flgs.x, uint8) ::
          conditional(flgs.y, uint8) ::
          conditional(flgs.z, uint8)
      }
      val values = valuesWithFlags.deriveElement {
        case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined)
      }
      values.encode(None, None, None) shouldBe Attempt.successful(bin"00000000")
      values.encode(Some(1), Some(2), Some(3)) shouldBe Attempt.successful(
        bin"11100000 00000001 00000010 00000011"
      )
      values.encode(Some(1), None, Some(3)) shouldBe Attempt.successful(
        bin"10100000 00000001 00000011"
      )
      roundtrip(values, (Some(1), Some(2), None))
    }

    "support alternative to flatPrepend+derive pattern that avoids intermediate codec shape" in {
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
      val values = flagsCodec.consume { flgs =>
        conditional(flgs.x, uint8) :: conditional(flgs.y, uint8) :: conditional(flgs.z, uint8)
      } { case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
      values.encode(None, None, None) shouldBe Attempt.successful(bin"00000000")
      values.encode(Some(1), Some(2), Some(3)) shouldBe Attempt.successful(
        bin"11100000 00000001 00000010 00000011"
      )
      values.encode(Some(1), None, Some(3)) shouldBe Attempt.successful(
        bin"10100000 00000001 00000011"
      )
      roundtrip(values, (Some(1), Some(2), None))
    }
  }
}
