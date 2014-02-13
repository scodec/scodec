package scodec
package codecs

import scalaz.syntax.id._
import scodec.bits.BitVector

class FloatCodecTest extends CodecSuite {
  test("float") { forAll { (n: Float) => roundtrip(float, n) } }

  test("floatL") { forAll { (n: Float) => roundtrip(floatL, n) } }

  test("endianness") {
    forAll { (n: Float) =>
      Codec.decode(floatL, float.encode(n).toOption.get.reverseByteOrder).toOption.get shouldBe n
      Codec.decode(float, floatL.encode(n).toOption.get.reverseByteOrder).toOption.get shouldBe n
    }
  }

  test("decoding with too few bits") {
    float.decode(BitVector.low(8)) shouldBe ("cannot acquire 32 bits from a vector that contains 8 bits".left)
  }
}
