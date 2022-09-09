package scodec
package codecs

import scodec.bits.BitVector

class DoubleCodecTest extends CodecSuite {

  "the double codec" should {
    "roundtrip" in {
      forAll { (n: Double) =>
        roundtrip(double, n)
      }
    }
  }
  "the doubleL codec" should {
    "roundtrip" in {
      forAll { (n: Double) =>
        roundtrip(doubleL, n)
      }
    }
  }

  "the double codecs" should {

    "support endianness correctly" in {
      forAll { (n: Double) =>
        doubleL.decode(double.encode(n).require.reverseByteOrder).require.value shouldBe n
        double.decode(doubleL.encode(n).require.reverseByteOrder).require.value shouldBe n
      }
    }

    "return an error when decoding with too few bits" in {
      double.decode(BitVector.low(8)) shouldBe Attempt.failure(Err.insufficientBits(64, 8))
    }
  }
}
