package scodec
package codecs

import scodec.bits.BitVector
import org.scalacheck.Prop.forAll

class DoubleCodecTest extends CodecSuite {

  property("double - roundtrip") {
    forAll((n: Double) => roundtrip(double, n))
  }

  property("doubleL - roundtrip") {
    forAll((n: Double) => roundtrip(doubleL, n))
  }

  property("support endianness correctly") {
    forAll { (n: Double) =>
      assertEquals(doubleL.decode(double.encode(n).require.reverseByteOrder).require.value, n)
      assertEquals(double.decode(doubleL.encode(n).require.reverseByteOrder).require.value, n)
    }
  }

  test("return an error when decoding with too few bits") {
    assertEquals(double.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(64, 8)))
  }
}
