package scodec

import scalaz.{\/, -\/, \/-}
import \/.{ right, left }

import scodec.bits._
import scodec.codecs._
import shapeless._
import shapeless.record._
import shapeless.union._
import shapeless.syntax.singleton._

class DerivedCodecTest extends CodecSuite {

  sealed trait Parent
  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent

  case class Qux(bar: Bar)
  case class Quy(x: Int, bar: Bar)
  case class Quz(x: Int, y: String, bars: Vector[Bar])

  case class Point(x: Int, y: Int, z: Int)
  case class Woz(x: Int, y: String, pts: Vector[Point])

  "automatic codec generation" should {
    "support automatic generation of HList codecs" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      Codec.derive[Int :: Int :: String :: HNil].encodeValid(1 :: 2 :: "Hello" :: HNil) shouldBe hex"0102000548656c6c6f".bits
    }

    "support automatic generation of case class codecs" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      Codec.derive[Foo].encodeValid(Foo(1, 2, "Hello")) shouldBe hex"0102000548656c6c6f".bits
    }

    "support automatic generation of nested case class codecs, where component codecs are derived as well" in {
      import implicits._
      // This shouldn't be necessary but it is required to avoid diverging implicit expansion
      implicit val bar = Codec.derive[Bar]
      Codec.derive[Qux]
      Codec.derive[Quy]
      Codec.derive[Quz]
      // Again, it shouldn't be necessary to put the Codec[Point] in to implicit scope, because derivation of Woz
      // should be able to derive Point too -- but it is required
      implicit val pt = Codec.derive[Point]
      Codec.derive[Woz]
    }

    "include field names in case class codecs" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      Codec.derive[Foo].encode(Foo(1, 256, "Hello")) shouldBe left(Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("y"))
    }

    "support automatic generation of coproduct codec builders" in {
      implicit val (u, s) = (constant(1), variableSizeBytes(uint16, utf8))
      type C = Unit :+: String :+: CNil
      val codec = Codec.coproduct[C].choice
      codec.encodeValid(Coproduct[C]("Hello")) shouldBe hex"000548656c6c6f".bits
      codec.encodeValid(Coproduct[C](())) shouldBe hex"01".bits
    }

    "support automatic generation of coproduct codec builders from union types" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      val uSchema = RecordType.like('i ->> 24 :: 's ->> "foo" :: HNil)
      type U = uSchema.Union
      val codec = Codec.coproduct[U].discriminatedByIndex(uint8)
      codec.encodeValid(Coproduct[U]('s ->> "Hello")) shouldBe hex"01000548656c6c6f".bits
      codec.encode(Coproduct[U]('i ->> 256)) shouldBe left(Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("i"))
    }

    "support automatic generation of coproduct codec builders from sealed trait and subclasses" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      val codec: Codec[Parent] = Codec.coproduct[Parent].discriminatedByIndex(uint8)
      codec.encodeValid(Foo(1, 2, "Hello")) shouldBe hex"010102000548656c6c6f".bits
      codec.encodeValid(Bar(1, 2)) shouldBe hex"000102".bits
    }
  }
}
