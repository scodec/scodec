package scodec
package examples

import scodec.bits._
import codecs._
import implicits._

class DerivedCodecsExample extends CodecSuite {

  sealed trait Sprocket
  object Sprocket {
    implicit val discriminated: Discriminated[Sprocket, Int] = Discriminated(uint8)
  }

  case class Woozle(count: Int, strength: Int) extends Sprocket
  object Woozle {
    implicit val discriminator: Discriminator[Sprocket, Woozle, Int] = Discriminator(1)
  }

  case class Wocket(size: Int, inverted: Boolean) extends Sprocket
  object Wocket {
    implicit val discriminator: Discriminator[Sprocket, Wocket, Int] = Discriminator(2)
  }

  case class Wootle(count: Int, data: BitVector) extends Sprocket
  object Wootle {
    implicit val discriminator: Discriminator[Sprocket, Wootle, Int] = Discriminator(3)
    implicit val codec: Codec[Wootle] = (uint8 :: bits).as[Wootle]
  }

  case class Geiling(name: String, sprockets: Vector[Sprocket])

  sealed trait Color
  object Color {
    case object Red extends Color
    case object Yellow extends Color
    case object Green extends Color

    implicit val d: Discriminated[Color, Int] = Discriminated(uint8)

    implicit val codec: Codec[Color] = mappedEnum(uint8, Red -> 0, Yellow -> 1, Green -> 2)
  }

  case class Point(x: Int, y: Int, z: Int)
  object Point {
    implicit val codec: Codec[Point] = (uint8 :: uint8 :: uint8).as[Point]
  }

  "derived codec examples" should {

    "demonstrate deriving a codec for a case class" in {
      // Codecs can be derived automatically for case classes where each component
      // type has an implicit codec available.
      //
      // In this example, Woozle is a product of two integers, and scodec.codecs.implicits._
      // is imported in this file, resulting an an implicit Codec[Int] being available.
      Codec[Woozle].encode(Woozle(1, 2)).require shouldBe hex"0000000100000002".bits
    }

    "demonstrate deriving a codec for a sealed class hierarchy" in {
      // Codecs can be derived automatically for sealed class hierarchies where:
      //  - there is an implicit Discriminated[R, D] available where R is the root of
      //    the type hierarchy and D is the type of the discriminator
      //  - there is an implicit Discriminator[R, X, D] available for each subtype X of R
      //  - each subtype has an implicit codec (or can have one derived)
      //
      // In this example, Sprocket defines a Discriminated[Sprocket, Int] in its companion
      // and each subclass defines a Discriminator[Sprocket, X, Int] in their companions.
      Codec[Sprocket].encode(Wocket(3, true)).require shouldBe hex"0200000003ff".bits
    }

    "demonstrate overriding arbitrary subtype codecs" in {
      // When deriving a codec for a sealed class hierarchy, the codec used for each
      // subtype is looked up implicitly, so they can be overriden by manipulating
      // implicit scope.
      //
      // For example, we can change the Wocket codec and derive a new Sprocket codec.
      implicit val codec: Codec[Wocket] = (uint8 :: ignore(7) :: bool).dropUnits.as[Wocket]
      Codec[Sprocket].encode(Wocket(3, true)).require shouldBe hex"020301".bits
    }

    "demonstrate subtype overrides in companion" in {
      // Alternatively, the overriden codec can be defined in the companion of the subtype.
      Codec[Sprocket].encode(Wootle(4, hex"deadbeef".bits)).require shouldBe hex"0304deadbeef".bits
    }

    "demonstrate nested derivations" in {
      // Derived codecs can be based on other derived codecs.
      //
      // Geiling has a Vector[Sprocket] element. The scodec.codecs.implicits object
      // defines an implicit codec for Vector[A] when there's an implicitly available
      // Codec[A]. There's no manually defined `Codec[Sprocket]` but one is implicitly
      // derived.
      val ceil = Geiling("Ceil", Vector(Woozle(1, 2), Wocket(3, true)))
      val encoded = Codec[Geiling].encode(ceil).require
      encoded shouldBe hex"00000004 4365696c 00000002 010000000100000002 0200000003ff".bits
      Codec[Geiling].decode(encoded).require.value shouldBe ceil
    }

    "demonstrate that derivation support does not interfere with manually authored implicit codecs in companions" in {
      Codec[Color].encode(Color.Green).require shouldBe hex"02".bits
      Codec[Point].encode(Point(1, 2, 3)).require should have size (24)
    }
  }
}
