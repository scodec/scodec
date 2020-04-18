package scodec
package codecs

import scodec.bits.BitVector
import org.scalacheck.Prop.forAll

class FloatCodecTest extends CodecSuite {

  test("float - roundtrip") {
    forAll((n: Float) => roundtrip(float, n))
  }
  test("floatL - roundtrip") {
    forAll((n: Float) => roundtrip(floatL, n))
  }

  property("support endianness correctly") {
    forAll { (n: Float) =>
      assertEquals(floatL.decode(float.encode(n).require.reverseByteOrder).require.value, n)
      assertEquals(float.decode(floatL.encode(n).require.reverseByteOrder).require.value, n)
    }
  }

  test("return an error when decoding with too few bits") {
    assertEquals(float.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(32, 8)))
  }
}
