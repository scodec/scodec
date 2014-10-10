package scodec
package codecs

import scalaz.\/
import scodec.bits.BitVector

class DoubleCodecTest extends CodecSuite {

  "the double codec" should { "roundtrip" in { forAll { (n: Double) => roundtrip(double, n) } } }
  "the doubleL codec" should { "roundtrip" in { forAll { (n: Double) => roundtrip(doubleL, n) } } }

  "the double codecs" should {

    "support endianness correctly" in {
      forAll { (n: Double) =>
        doubleL.decodeValidValue(double.encodeValid(n).reverseByteOrder) shouldBe n
        double.decodeValidValue(doubleL.encodeValid(n).reverseByteOrder) shouldBe n
      }
    }

    "return an error when decoding with too few bits" in {
      double.decode(BitVector.low(8)) shouldBe \/.left(Err.insufficientBits(64, 8))
    }
  }
}

