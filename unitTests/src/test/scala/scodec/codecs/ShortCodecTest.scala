package scodec
package codecs

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class ShortCodecTest extends CodecSuite {
  def check(low: Short, high: Short)(f: Short => Unit) =
    forAll(Gen.choose(low, high))(n => f(n))

  property("short16 - roundtrip") {
    forAll((n: Short) => roundtrip(short16, n))
  }

  property("short16L - roundtrip") {
    forAll((n: Short) => roundtrip(short16L, n))
  }

  property("ushort(n) - roundtrip") {
    forAll(Gen.choose(0, 32767))(n => roundtrip(ushort(15), n.toShort))
  }

  property("ushortL(n) - roundtrip") {
    forAll(Gen.choose(0, 32767))(n => roundtrip(ushortL(15), n.toShort))
  }

  property("support endianess correctly - (1)") {
    forAll { (n: Short) =>
      val bigEndian = short16.encode(n).require.toByteVector
      val littleEndian = short16L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }

  property("support endianess correctly - (2)") {
    check(0, 8191) { (n: Short) =>
      val bigEndian = ushort(13).encode(n).require
      val littleEndian = ushortL(13).encode(n).require.toByteVector
      val flipped = BitVector(littleEndian.last).take(5) ++ littleEndian.init.reverse.toBitVector
      assertEquals(flipped, bigEndian)
    }
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(short(15).encode(Short.MaxValue), Attempt.failure(
      Err("32767 is greater than maximum value 16383 for 15-bit signed short")
    ))
    assertEquals(short(15).encode(Short.MinValue), Attempt.failure(
      Err("-32768 is less than minimum value -16384 for 15-bit signed short")
    ))
    assertEquals(ushort(15).encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 15-bit unsigned short")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(short16.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(16, 8)))
  }
}
