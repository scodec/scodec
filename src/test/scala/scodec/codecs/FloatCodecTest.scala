package scodec
package codecs

import scalaz.syntax.id._
import scodec.bits.BitVector

class FloatCodecTest extends CodecSuite {
  test("float") { forAll { (n: Float) => roundtrip(float, n) } }

  test("floatL") { forAll { (n: Float) => roundtrip(floatL, n) } }

  test("endianness") {
    forAll { (n: Float) =>
      floatL.decodeValidValue(float.encodeValid(n).reverseByteOrder) shouldBe n
      float.decodeValidValue(floatL.encodeValid(n).reverseByteOrder) shouldBe n
    }
  }

  test("decoding with too few bits") {
    float.decode(BitVector.low(8)) shouldBe ("cannot acquire 32 bits from a vector that contains 8 bits".left)
  }
}
