package scodec
package codecs

import shapeless._

import scodec.bits._

class CoproductCodecTest extends CodecSuite {

  "coproduct codec support" should {

    "support index based discriminators" in {
      type IBS = Int :+: Boolean :+: String :+: CNil
      val codec: Codec[IBS] = (int32 :+: bool(8) :+: variableSizeBytes(uint8, ascii)).discriminatedByIndex(uint8)

      codec.encode(Coproduct[IBS](1)).require shouldBe hex"0000000001".bits
      codec.encode(Coproduct[IBS](true)).require shouldBe hex"01ff".bits
      codec.encode(Coproduct[IBS]("Hello")).require shouldBe hex"020548656c6c6f".bits

      forAll { (i: Int) => roundtrip(codec, Coproduct[IBS](i)) }
      forAll { (b: Boolean) => roundtrip(codec, Coproduct[IBS](b)) }
      forAll { (s: String) =>
        if (s.size < 256) {
          codec.encode(Coproduct[IBS](s)).toOption.foreach { bits =>
            codec.complete.decode(bits).require shouldBe Coproduct[IBS](s)
          }
        }
      }
    }

    "support arbitrary discriminators" in {
      type IBS = Int :+: Boolean :+: String :+: CNil
      val codec: Codec[IBS] =
        (int32 :+: bool(8) :+: variableSizeBytes(uint8, ascii)).
          discriminatedBy(fixedSizeBytes(1, ascii)).
          using(Sized("i", "b", "s"))

      codec.encode(Coproduct[IBS](1)).require shouldBe hex"6900000001".bits
      codec.encode(Coproduct[IBS](true)).require shouldBe hex"62ff".bits
      codec.encode(Coproduct[IBS]("Hello")).require shouldBe hex"730548656c6c6f".bits

      forAll { (i: Int) => roundtrip(codec, Coproduct[IBS](i)) }
      forAll { (b: Boolean) => roundtrip(codec, Coproduct[IBS](b)) }
      forAll { (s: String) =>
        if (s.size < 256) {
          codec.encode(Coproduct[IBS](s)).toOption.foreach { bits =>
            codec.complete.decode(bits).require shouldBe Coproduct[IBS](s)
          }
        }
      }
    }

    "support choice codecs" in {
      type US = Unit :+: String :+: CNil
      val codec: Codec[US] =
        (constant(1) :+: variableSizeBytes(uint8, ascii)).choice

      codec.encode(Coproduct[US](())).require shouldBe hex"01".bits
      codec.complete.decode(hex"01".bits).require shouldBe Coproduct[US](())

      codec.encode(Coproduct[US]("Hello")).require shouldBe hex"0548656c6c6f".bits
      codec.complete.decode(hex"0548656c6c6f".bits).require shouldBe Coproduct[US]("Hello")
    }
  }
}
