package scodec
package examples

import shapeless._

import scodec.bits._
import codecs._

class ProductsExample extends CodecSuite {

  case class Woozle(count: Int, strength: Int)
  case class Wocket(size: Int, inverted: Boolean)

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

      codec.encode(Woozle(1, 256)) shouldBe EncodeResult.failure(Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("strength"))
    }

    "demonstrate hlist generation" in {
      // Codec.product supports direct `HList` types as well.
      implicit val (i, b) = (uint8, bool)
      val codec = Codec[Int :: Int :: Boolean :: HNil]

      codec.encode(1 :: 2 :: true :: HNil).require shouldBe hex"0102ff".bits.dropRight(7)
    }
  }
}
