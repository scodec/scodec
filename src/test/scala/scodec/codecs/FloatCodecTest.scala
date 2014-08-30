package scodec
package codecs

import scalaz.\/
import scodec.bits.BitVector

class FloatCodecTest extends CodecSuite {

  "the float codec" should { "roundtrip" in { forAll { (n: Float) => roundtrip(float, n) } } }
  "the floatL codec" should { "roundtrip" in { forAll { (n: Float) => roundtrip(floatL, n) } } }

  "the float codecs" should {

    "support endianness correctly" in {
      forAll { (n: Float) =>
        floatL.decodeValidValue(float.encodeValid(n).reverseByteOrder) shouldBe n
        float.decodeValidValue(floatL.encodeValid(n).reverseByteOrder) shouldBe n
      }
    }

    "return an error when decoding with too few bits" in {
      float.decode(BitVector.low(8)) shouldBe \/.left("cannot acquire 32 bits from a vector that contains 8 bits")
    }
  }
}
