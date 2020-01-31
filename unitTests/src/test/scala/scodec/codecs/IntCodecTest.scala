package scodec
package codecs

import org.scalacheck.Gen
import scodec.bits.BitVector

class IntCodecTest extends CodecSuite {
  def check[A](low: Int, high: Int)(f: (Int) => A): Unit =
    forAll(Gen.choose(low, high)) { n =>
      f(n)
    }

  "the int32 codec" should {
    "roundtrip" in {
      forAll { (n: Int) =>
        roundtrip(int32, n)
      }
    }
  }
  "the int32L codec" should {
    "roundtrip" in {
      forAll { (n: Int) =>
        roundtrip(int32L, n)
      }
    }
  }
  "the uint24L codec" should {
    "roundtrip" in {
      check(0, (1 << 24) - 1) { (n: Int) =>
        roundtrip(uint24L, n)
      }
    }
  }
  "the int16 codec" should {
    "roundtrip" in {
      check(-32768, 32767) { (n: Int) =>
        roundtrip(int16, n)
      }
    }
  }
  "the uint16 codec" should {
    "roundtrip" in {
      check(0, 65535) { (n: Int) =>
        roundtrip(uint16, n)
      }
    }
  }
  "the uint16L codec" should {
    "roundtrip" in {
      check(0, 65535) { (n: Int) =>
        roundtrip(uint16L, n)
      }
    }
  }
  "the uint8 codec" should {
    "roundtrip" in {
      check(0, 255) { (n: Int) =>
        roundtrip(uint8, n)
      }
    }
  }
  "the uint8L codec" should {
    "roundtrip" in {
      check(0, 255) { (n: Int) =>
        roundtrip(uint8L, n)
      }
    }
  }
  "the uint4 codec" should {
    "roundtrip" in {
      check(0, 1 << 3) { (n: Int) =>
        roundtrip(uint4, n)
      }
    }
  }
  "the uint4L codec" should {
    "roundtrip" in {
      check(0, (1 << 4) - 1) { (n: Int) =>
        roundtrip(uint4L, n)
      }
    }
  }
  "the uint(n) codec" should {
    "roundtrip" in {
      uint(13).encode(1) shouldBe Attempt.successful(BitVector.low(13).set(12))
      check(0, 32767) { (n: Int) =>
        roundtrip(uint(15), n)
      }
    }
  }
  "the uintL(n) codec" should {
    "roundtrip" in {
      uintL(13).encode(1) shouldBe Attempt.successful(BitVector.low(13).set(7))
      check(0, 32767) { (n: Int) =>
        roundtrip(uintL(15), n)
      }
    }
  }

  "the int codecs" should {
    "support endianess correctly" in {
      forAll { (n: Int) =>
        val bigEndian = int32.encode(n).require.toByteVector
        val littleEndian = int32L.encode(n).require.toByteVector
        littleEndian shouldBe bigEndian.reverse
      }
      check(0, 15) { (n: Int) =>
        val bigEndian = uint4.encode(n).require.toByteVector
        val littleEndian = uint4L.encode(n).require.toByteVector
        littleEndian shouldBe bigEndian.reverse
      }
      check(0, (1 << 24) - 1) { (n: Int) =>
        val bigEndian = uint24.encode(n).require.toByteVector
        val littleEndian = uint24L.encode(n).require.toByteVector
        littleEndian shouldBe bigEndian.reverse
      }
      check(0, 8191) { (n: Int) =>
        val bigEndian = uint(13).encode(n).require
        val littleEndian = uintL(13).encode(n).require.toByteVector
        val flipped = BitVector(littleEndian.last).take(5) ++ littleEndian.init.reverse.toBitVector
        flipped shouldBe bigEndian
      }
    }

    "return an error when value to encode is out of legal range" in {
      int16.encode(65536) shouldBe Attempt.failure(
        Err("65536 is greater than maximum value 32767 for 16-bit signed integer")
      )
      int16.encode(-32769) shouldBe Attempt.failure(
        Err("-32769 is less than minimum value -32768 for 16-bit signed integer")
      )
      uint16.encode(-1) shouldBe Attempt.failure(
        Err("-1 is less than minimum value 0 for 16-bit unsigned integer")
      )
    }

    "return an error when decoding with too few bits" in {
      int16.decode(BitVector.low(8)) shouldBe Attempt.failure(Err.insufficientBits(16, 8))
    }
  }
}
