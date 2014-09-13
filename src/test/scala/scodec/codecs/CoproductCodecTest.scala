package scodec
package codecs

import shapeless._

import scodec.bits._

class CoproductCodecTest extends CodecSuite {

  "coproduct codec support" should {

    "support index based discriminators" in {
      type IBS = Int :+: Boolean :+: String :+: CNil
      val codec: Codec[IBS] = (int32 :+: bool(8) :+: variableSizeBytes(uint8, ascii)).discriminatedByIndex(uint8)

      codec.encodeValid(Coproduct[IBS](1)) shouldBe hex"0000000001".bits
      codec.encodeValid(Coproduct[IBS](true)) shouldBe hex"01ff".bits
      codec.encodeValid(Coproduct[IBS]("Hello")) shouldBe hex"020548656c6c6f".bits

      forAll { (i: Int) => roundtrip(codec, Coproduct[IBS](i)) }
      forAll { (b: Boolean) => roundtrip(codec, Coproduct[IBS](b)) }
      forAll { (s: String) =>
        if (s.size < 256) {
          codec.encode(Coproduct[IBS](s)) foreach { bits =>
            codec.complete.decodeValidValue(bits) shouldBe Coproduct[IBS](s)
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

      codec.encodeValid(Coproduct[IBS](1)) shouldBe hex"6900000001".bits
      codec.encodeValid(Coproduct[IBS](true)) shouldBe hex"62ff".bits
      codec.encodeValid(Coproduct[IBS]("Hello")) shouldBe hex"730548656c6c6f".bits

      forAll { (i: Int) => roundtrip(codec, Coproduct[IBS](i)) }
      forAll { (b: Boolean) => roundtrip(codec, Coproduct[IBS](b)) }
      forAll { (s: String) =>
        if (s.size < 256) {
          codec.encode(Coproduct[IBS](s)) foreach { bits =>
            codec.complete.decodeValidValue(bits) shouldBe Coproduct[IBS](s)
          }
        }
      }
    }

    "support choice codecs" in {
      type US = Unit :+: String :+: CNil
      val codec: Codec[US] =
        (constant(1) :+: variableSizeBytes(uint8, ascii)).choice

      codec.encodeValid(Coproduct[US](())) shouldBe hex"01".bits
      codec.complete.decodeValidValue(hex"01".bits) shouldBe Coproduct[US](())

      codec.encodeValid(Coproduct[US]("Hello")) shouldBe hex"0548656c6c6f".bits
      codec.complete.decodeValidValue(hex"0548656c6c6f".bits) shouldBe Coproduct[US]("Hello")
    }
  }
}
