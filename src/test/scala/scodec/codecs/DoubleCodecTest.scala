package scodec
package codecs

import scalaz.syntax.id._
import scodec.bits.BitVector

class DoubleCodecTest extends CodecSuite {
  test("double") { forAll { (n: Double) => roundtrip(double, n) } }

  test("doubleL") { forAll { (n: Double) => roundtrip(doubleL, n) } }

  test("endianness") {
    forAll { (n: Double) =>
      doubleL.decodeValidValue(double.encodeValid(n).reverseByteOrder) shouldBe n
      double.decodeValidValue(doubleL.encodeValid(n).reverseByteOrder) shouldBe n
    }
  }

  test("decoding with too few bits") {
    double.decode(BitVector.low(8)) shouldBe ("cannot acquire 64 bits from a vector that contains 8 bits".left)
  }
}

