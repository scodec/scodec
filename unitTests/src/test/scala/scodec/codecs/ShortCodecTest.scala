package scodec
package codecs

import org.scalacheck.Gen
import scodec.bits.BitVector

class ShortCodecTest extends CodecSuite {
  def check[A](low: Short, high: Short)(f: (Short) => A) =
    forAll(Gen.choose(low, high)) { n =>
      f(n)
    }

  "the short16 codec" should {
    "roundtrip" in {
      forAll { (n: Short) =>
        roundtrip(short16, n)
      }
    }
  }
  "the short16L codec" should {
    "roundtrip" in {
      forAll { (n: Short) =>
        roundtrip(short16L, n)
      }
    }
  }
  "the ushort(n) codec" should {
    "roundtrip" in {
      forAll(Gen.choose(0, 32767)) { n =>
        roundtrip(ushort(15), n.toShort)
      }
    }
  }
  "the ushortL(n) codec" should {
    "roundtrip" in {
      forAll(Gen.choose(0, 32767)) { n =>
        roundtrip(ushortL(15), n.toShort)
      }
    }
  }

  "the short codecs" should {
    "support endianess correctly" in {
      forAll { (n: Short) =>
        val bigEndian = short16.encode(n).require.toByteVector
        val littleEndian = short16L.encode(n).require.toByteVector
        littleEndian shouldBe bigEndian.reverse
      }
      check(0, 8191) { (n: Short) =>
        val bigEndian = ushort(13).encode(n).require
        val littleEndian = ushortL(13).encode(n).require.toByteVector
        val flipped = BitVector(littleEndian.last).take(5) ++ littleEndian.init.reverse.toBitVector
        flipped shouldBe bigEndian
      }
    }

    "return an error when value to encode is out of legal range" in {
      short(15).encode(Short.MaxValue) shouldBe Attempt.failure(
        Err("32767 is greater than maximum value 16383 for 15-bit signed short")
      )
      short(15).encode(Short.MinValue) shouldBe Attempt.failure(
        Err("-32768 is less than minimum value -16384 for 15-bit signed short")
      )
      ushort(15).encode(-1) shouldBe Attempt.failure(
        Err("-1 is less than minimum value 0 for 15-bit unsigned short")
      )
    }

    "return an error when decoding with too few bits" in {
      short16.decode(BitVector.low(8)) shouldBe Attempt.failure(Err.insufficientBits(16, 8))
    }
  }
}
