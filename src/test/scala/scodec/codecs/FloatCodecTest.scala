package scodec
package codecs

import scodec.bits.BitVector

class FloatCodecTest extends CodecSuite {

  "the float codec" should { "roundtrip" in { forAll { (n: Float) => roundtrip(float, n) } } }
  "the floatL codec" should { "roundtrip" in { forAll { (n: Float) => roundtrip(floatL, n) } } }

  "the float codecs" should {

    "support endianness correctly" in {
      forAll { (n: Float) =>
        floatL.decode(float.encodeValid(n).reverseByteOrder).require shouldBe n
        float.decode(floatL.encodeValid(n).reverseByteOrder).require shouldBe n
      }
    }

    "return an error when decoding with too few bits" in {
      float.decode(BitVector.low(8)) shouldBe DecodeResult.failure(Err.insufficientBits(32, 8))
    }
  }
}
