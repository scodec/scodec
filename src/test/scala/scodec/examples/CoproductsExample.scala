package scodec
package examples

import scalaz.\/
import shapeless._

import scodec.bits._
import codecs._

class CoproductsExample extends CodecSuite {

  sealed trait Sprocket

  case class Woozle(count: Int, strength: Int) extends Sprocket
  object Woozle {
    implicit val codec: Codec[Woozle] = (uint8 :: uint8).as[Woozle]
  }

  case class Wocket(size: Int, inverted: Boolean) extends Sprocket
  object Wocket {
    implicit val codec: Codec[Wocket] = (uint8 :: ignore(7) :: bool).dropUnits.as[Wocket]
  }

  "coproduct codec examples" should {
    // Codec.coproduct[Sprocket] returns a `CoproductCodecBuilder` which lets
    // us specify how the various subtypes are distinguished from each other
    // in the binary format.
    //
    // It requires that each subtype has an implicit codec available.
    // In this case, both `Woozle` and `Wocket` have implicit codecs
    // in their companions.

    "demonstrate choice" in {
      // The simplest way to create a codec from the builder is to use the
      // `choice` combinator, which does not include a discriminator in the
      // binary -- instead, each component codec is tried in turn and the first
      // successful response is used.
      val choiceCodec: Codec[Sprocket] = Codec.coproduct[Sprocket].choice

      val encodedWoozle = choiceCodec.encodeValid(Woozle(3, 10))
      val encodedWocket = choiceCodec.encode(Wocket(1, true))

      // Care must be taken when using choice codecs -- specifically,
      // if a specific bit pattern can be successfully decoded by multiple
      // component codecs, decoding results may be unexpected.
      choiceCodec.decodeValidValue(encodedWoozle) shouldBe Wocket(3, false)

      // In the previous example, the encoded woozle bits decode to a `Wocket`
      // because of the order that Shapeless enumerates the subtypes of `Sprocket`
      // in the coproduct output type of `LabelledGeneric[Sprocket]`. Many
      // of the combinators that let us build a codec from a `CoproductCodecBuilder`
      // are dependent on the order of the component types.
    }

    // The coprodocut codec builder provides a number of ways to build discriminated
    // codecs -- that is, codecs in which the bit pattern includes a value that
    // indicates what coproduct component type follows the discriminator.

    "demonstrate index based disciminators" in {
      // The simplest discriminator type is index based. Given that a coproduct
      // is represented as `C0 :+: C1 :+: ... :+: Cn :+: CNil`, the index is
      // `i` in `Ci`. For example, the `Sprocket` type is represented by the coproduct
      // `Wocket :+: Woozle :+: CNil`. Hence, `Wocket` is at index 0 and `Woozle`
      // is at index 1.
      //
      // Saying that the coproduct is discriminated by its index is not sufficient
      // to describe the bit pattern though. We need to describe how the index
      // is represented. This is done by passing a `Codec[Int]` to the
      // `discriminatedByIndex` method.
      val codec: Codec[Sprocket] = Codec.coproduct[Sprocket].discriminatedByIndex(uint8)

      val encodedWoozle = codec.encodeValid(Woozle(3, 10))
      encodedWoozle shouldBe hex"01030a".bits
      val encodedWocket = codec.encodeValid(Wocket(1, true))
      encodedWocket shouldBe hex"000101".bits

      codec.decodeValidValue(encodedWoozle) shouldBe Woozle(3, 10)
      codec.decodeValidValue(encodedWocket) shouldBe Wocket(1, true)
    }

    "demonstrate errors" in {
      // All union based coproduct codecs include the field name in errors
      // that occur in encoding and decoding. For codecs generated from
      // sealed type hierarchies, the field name is the subtype name.
      val codec: Codec[Sprocket] = Codec.coproduct[Sprocket].discriminatedByIndex(uint8)

      codec.encode(Woozle(256, 0)) shouldBe \/.left(Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("Woozle"))
    }

    "demonstrate arbitrary discriminators" in {
      // Index based discriminators are typically not useful when working with
      // binary formats defined by protocols. Instead, an arbitrary discriminator
      // can be associated with each component type. The discriminator values
      // are provided by passing a sized collection with a size matching the number
      // of components in the coproduct. Each value in the collection is used
      // as the discriminator for the component at the corresponding index
      // in the coproduct.

      val codec: Codec[Sprocket] = Codec.coproduct[Sprocket].discriminatedBy(uint8).using(Sized(2, 1))

      val encodedWoozle = codec.encodeValid(Woozle(3, 10))
      encodedWoozle shouldBe hex"01030a".bits
      val encodedWocket = codec.encodeValid(Wocket(1, true))
      encodedWocket shouldBe hex"020101".bits

      codec.decodeValidValue(encodedWoozle) shouldBe Woozle(3, 10)
      codec.decodeValidValue(encodedWocket) shouldBe Wocket(1, true)

      // Notice that the following do not compile because the sized collection has the wrong size:
      """Codec.coproduct[Sprocket].discriminatedBy(uint8).using(Sized(2, 1, 3))""" shouldNot compile
      """Codec.coproduct[Sprocket].discriminatedBy(uint8).using(Sized(2))""" shouldNot compile
    }

    "demonstrate key based discriminators" in {
      // Rather than relying on an arbitrary index to bind a discriminator to a subtype,
      // the binding can be performed by key instead assuming the codec
      // is a union based codec. For sealed class hierarchies, this means the
      // subtype name can be used.
      import shapeless.syntax.singleton._

      val codec: Codec[Sprocket] = Codec.coproduct[Sprocket].discriminatedByKey(uint8,
        'Wocket ->> 2 :: 'Woozle ->> 1 :: HNil)

      val encodedWoozle = codec.encodeValid(Woozle(3, 10))
      encodedWoozle shouldBe hex"01030a".bits
      val encodedWocket = codec.encodeValid(Wocket(1, true))
      encodedWocket shouldBe hex"020101".bits

      codec.decodeValidValue(encodedWoozle) shouldBe Woozle(3, 10)
      codec.decodeValidValue(encodedWocket) shouldBe Wocket(1, true)

      // However, the discriminator records must be aligned with the coproduct types.
      // In a future version, this limitation will probably be lifted. As of now,
      // it is equal in power to `discriminatedBy(...).using(Sized(...))` but offers
      // better readability.
      """Codec.coproduct[Sprocket].discriminatedByKey(uint8, 'Woozle ->> 1 :: 'Wocket ->> 2 :: HNil)""" shouldNot compile
    }
  }
}
