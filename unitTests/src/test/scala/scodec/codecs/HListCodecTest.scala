package scodec
package codecs

import scodec.bits._

import shapeless._

class HListCodecTest extends CodecSuite {

  case class Foo(x: Int, y: Int, s: String)
  case class Bar(x: Int)
  case class Baz(a: Int, b: Int, c: Int, d: Int)
  case class Flags(x: Boolean, y: Boolean, z: Boolean)

  "HList codec support" should {

    "support construction via :: operator" in {
      roundtripAll((uint8 :: uint8 :: utf8), Seq(1 :: 2 :: "test" :: HNil))
    }

    "support conversion HList codec to a case class codec via as method" in {
      roundtripAll((uint8 :: uint8 :: utf8).as[Foo], Seq(Foo(1, 2, "test")))
    }

    "support conversion of non-HList codec to a case class codec via as method" in {
      roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
    }

    "support converting an hlist of codecs" in {
      val a: Codec[Int :: Long :: Boolean :: HNil] = (uint8 :: int64 :: bool :: HNil).toCodec
    }

    "provide a flatPrepend method" in {
      uint8.flatPrepend(n => bits(n.toLong).hlist)
      ()
    }

    "provide a flatZipHList method" in {
      uint8.flatZipHList(n => bits(n.toLong))
      ()
    }

    "provide ability to append via :+ operator" in {
      roundtrip(((uint8 :: uint8) :+ utf8).as[Foo], Foo(1, 2, "test"))
    }

    "provide ability to concatenate two HList codecs" in {
      roundtrip(((uint8 :: uint8) ::: (uint8 :: uint8)).as[Baz], Baz(1, 2, 3, 4))
    }

    "support HList equivalent of Codec#dropLeft" in {
      val codec = (uint8.unit(0) :~>: uint8.hlist).as[Bar]
      roundtrip(codec, Bar(1))
      codec.encode(Bar(1)).require should have size (16)
    }

    "support HList equivalent of Codec#dropLeft on a non-HList codec" in {
      uint8.unit(0) :~>: uint8
      ()
    }

    "support dropping all Unit values out of an HList codec" in {
      def ign(size: Int) = scodec.codecs.ignore(size.toLong)
      val codec = (uint8 :: ign(8) :: uint8 :: ign(8) :: utf8).dropUnits.as[Foo]
      roundtrip(codec, Foo(1, 2, "test"))
    }

    "support removing an element of an HList codec by type" in {
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
      val valuesWithFlags = flagsCodec.flatPrepend { flgs =>
        conditional(flgs.x, uint8) ::
          conditional(flgs.y, uint8) ::
          conditional(flgs.z, uint8)
      }
      val values = valuesWithFlags.derive[Flags].from {
        case x :: y :: z :: HNil => Flags(x.isDefined, y.isDefined, z.isDefined)
      }
      values.encode(None :: None :: None :: HNil) shouldBe Attempt.successful(bin"00000000")
      values.encode(Some(1) :: Some(2) :: Some(3) :: HNil) shouldBe Attempt.successful(
        bin"11100000 00000001 00000010 00000011"
      )
      values.encode(Some(1) :: None :: Some(3) :: HNil) shouldBe Attempt.successful(
        bin"10100000 00000001 00000011"
      )
      roundtrip(values, Some(1) :: Some(2) :: None :: HNil)
    }

    "support alternative to flatPrepend+derive pattern that avoids intermediate codec shape" in {
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
      val values = flagsCodec.consume { flgs =>
        conditional(flgs.x, uint8) :: conditional(flgs.y, uint8) :: conditional(flgs.z, uint8)
      } {
        case x :: y :: z :: HNil => Flags(x.isDefined, y.isDefined, z.isDefined)
      }
      values.encode(None :: None :: None :: HNil) shouldBe Attempt.successful(bin"00000000")
      values.encode(Some(1) :: Some(2) :: Some(3) :: HNil) shouldBe Attempt.successful(
        bin"11100000 00000001 00000010 00000011"
      )
      values.encode(Some(1) :: None :: Some(3) :: HNil) shouldBe Attempt.successful(
        bin"10100000 00000001 00000011"
      )
      roundtrip(values, Some(1) :: Some(2) :: None :: HNil)
    }

    "support mapping a pair of polymorphic functions over an HList codec" in {
      object double extends Poly1 {
        implicit val i = at[Int](_ * 2)
        implicit val l = at[Long](_ * 2)
        implicit val b = at[Boolean](b => b)
      }
      object half extends Poly1 {
        implicit val i = at[Int](_ / 2)
        implicit val l = at[Long](_ / 2)
        implicit val b = at[Boolean](b => b)
      }
      val codec = (uint8 :: uint32 :: bool(8) :: uint8).polyxmap(half, double)

      val value = 1 :: 2L :: true :: 3 :: HNil

      val bits = codec.encode(value).require
      bits shouldBe hex"0200000004ff06".bits

      val decoded = codec.compact.decode(bits).require.value
      decoded shouldBe value
    }

    "support mapping a pair of polymorphic functions over a non-HList codec" in {
      object double extends Poly1 {
        implicit val i = at[Int](_ * 2)
        implicit val l = at[Long](_ * 2)
        implicit val b = at[Boolean](b => b)
      }
      object half extends Poly1 {
        implicit val i = at[Int](_ / 2)
        implicit val l = at[Long](_ / 2)
        implicit val b = at[Boolean](b => b)
      }
      val codec = uint8.polyxmap(half, double)

      val value = 1

      val bits = codec.encode(value).require
      bits shouldBe hex"02".bits

      val decoded = codec.compact.decode(bits).require.value
      decoded shouldBe value
    }

    "support mapping a single polymorphic function over an HList codec" in {
      object negate extends Poly1 {
        implicit val i = at[Int](i => -i)
        implicit val l = at[Long](i => -i)
        implicit val b = at[Boolean](b => b)
      }
      val codec = (uint8 :: uint32 :: bool(8) :: uint8).polyxmap1(negate)

      val value = -1 :: -2L :: true :: -3 :: HNil

      val bits = codec.encode(value).require
      bits shouldBe hex"0100000002ff03".bits

      val decoded = codec.compact.decode(bits).require.value
      decoded shouldBe value
    }

    "support mapping a single polymorphic function over a non-HList codec" in {
      object i2d extends Poly1 {
        implicit val i = at[Int](_.toDouble)
        implicit val d = at[Double](_.toInt)
      }
      val codec: Codec[Double] = uint8.polyxmap1(i2d)

      val value = 1.0d

      val bits = codec.encode(value).require
      bits shouldBe hex"01".bits

      val decoded = codec.compact.decode(bits).require.value
      decoded shouldBe value
    }
  }
}
