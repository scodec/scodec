package scodec
package examples

import shapeless._

import scodec.bits._
import codecs._

class ProductsExample extends CodecSuite {

  case class Woozle(count: Int, strength: Int)
  case class Wocket(size: Int, inverted: Boolean)
  case class Flags(x: Boolean, y: Boolean, z: Boolean)

  "product codec examples" should {
    "demonstrate case class generation" in {
      // Codec.product generates `HList` codecs for the specified type
      // as long as there's an implicit codec available for each
      // component type.
      implicit val i = uint8
      val codec = Codec[Woozle]

      codec.encode(Woozle(1, 2)).require shouldBe hex"0102".bits

      // The following does not compile because there's no implicit
      // Codec[Boolean] in scope:
      """Codec.product[Wocket]""" shouldNot compile
    }

    "demonstrate errors in case class codecs are labelled" in {
      // Auto generated case class codecs label each component codec
      // with the field name from the case class.
      implicit val i = uint8
      val codec = Codec[Woozle]

      codec.encode(Woozle(1, 256)) shouldBe Attempt.failure(Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("strength"))
    }

    "demonstrate hlist generation" in {
      // Codec.product supports direct `HList` types as well.
      implicit val (i, b) = (uint8, bool)
      val codec = Codec[Int :: Int :: Boolean :: HNil]

      codec.encode(1 :: 2 :: true :: HNil).require shouldBe hex"0102ff".bits.dropRight(7)
    }

    "demonstrate use of flatPrepend to encode a dependency on a field" in {
      // Let's build a codec for the binary form:
      //  x_included              bool(1)
      //  y_included              bool(1)
      //  z_included              bool(1)
      //                          ignore(5)
      //  if (x_included) {
      //    x                     uint8
      //  }
      //  if (y_included) {
      //    y                     int64
      //  }
      //  if (z_included) {
      //    z                     utf8_32
      //  }

      // We can represent the first byte as a case class of three booleans:
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]

      // In order to decode the rest of the binary format, we need access to
      // the decoded flags -- we can do that with flatPrepend
      val codec = flagsCodec.flatPrepend { flgs =>
        conditional(flgs.x, uint8) :: conditional(flgs.y, int64) :: conditional(flgs.z, utf8_32)
      }

      // The type of codec is Flags prepended to whatever HList the body returned:
      val codecTyped: Codec[Flags :: Option[Int] :: Option[Long] :: Option[String] :: HNil] = codec

      val v = Flags(true, true, true) :: Some(1) :: Some(1L) :: Some("Hi") :: HNil
      codec.encode(v).require shouldBe (
        bin"11100000" ++ hex"010000000000000001000000024869".bits
      )

      // One disadvantage of this approach is that the HList is inhabited by illegal values.
      // For example, consider encoding the HList: Flags(true, true, true) :: None :: None :: None :: HNil.
      // In this example, the HList claims that x, y, and z are defined but then provides no values for those
      // fields. This is an example of allowing the binary structure leak in to the domain model.
    }

    "demonstrate use of consume as an alternative to flatPrepend" in {
      // To address the disadvantage we ran in to with flatPrepend, we can use consume instead.
      // The consume method is just like flatPrepend in the resulting binary format -- it differs
      // only in the return type of the codec.
      // We need to provide an additional function to consume -- one that is of type `L => A`, or
      // in this case, `Option[Int] :: Option[Long] :: Option[String] :: HNil => Flags`.
      // This function will be used to generate a Flags instance when encoding.
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
      val codec = flagsCodec.consume { flgs =>
        conditional(flgs.x, uint8) :: conditional(flgs.y, int64) :: conditional(flgs.z, utf8_32)
      } {
        case x :: y :: z :: HNil => Flags(x.isDefined, y.isDefined, z.isDefined)
      }
      val v = Some(1) :: Some(1L) :: Some("Hi") :: HNil
      codec.encode(v).require shouldBe (
        bin"11100000" ++ hex"010000000000000001000000024869".bits
      )
    }

    "demonstrate use of derive as an alternative to consume" in {
      // Sometimes, we have a Codec[X0 :: X1 :: ... :: Xn :: HNil] and we need to remove one of the
      // components, say Xi, resulting in a Codec[X0 :: ... :: Xi-1 :: Xi+1 :: ... :: Xn :: HNil],
      // while keeping the binary effect of Xi (that is, the encoding/decoding of Xi should still occur).
      // If Xi is dependent on a subset of the remaining components, we can use `derive` to mark
      // it as a derived field.
      val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
      val flagsAndFields = flagsCodec.flatPrepend { flgs =>
        conditional(flgs.x, uint8) :: conditional(flgs.y, int64) :: conditional(flgs.z, utf8_32)
      }
      val codec = flagsAndFields.derive[Flags].from {
        case x :: y :: z :: HNil => Flags(x.isDefined, y.isDefined, z.isDefined)
      }
      // In this example, we are deriving X0 for the shape X0 :: X1 :: X2 :: X3 :: HNil, but unlike consume,
      // derive works with any HList component -- not just the head.
      val v = Some(1) :: Some(1L) :: Some("Hi") :: HNil
      codec.encode(v).require shouldBe (
        bin"11100000" ++ hex"010000000000000001000000024869".bits
      )
    }
  }
}
