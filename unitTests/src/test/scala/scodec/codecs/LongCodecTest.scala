package scodec
package codecs

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class LongCodecTest extends CodecSuite {
  def check(low: Long, high: Long)(f: Long => Unit) =
    forAll(Gen.choose(low, high))(n => f(n))

  property("int64 - roundtrip") {
    forAll((n: Long) => roundtrip(int64, n))
  }

  property("inte64L - roundtrip") {
    forAll((n: Long) => roundtrip(int64L, n))
  }

  property("uint32 - roundtrip") {
    check(0, 1L << (32 - 1))((n: Long) => roundtrip(uint32, n))
  }

  property("uint32L - roundtrip") {
    check(0L, (1L << 32) - 1)((n: Long) => roundtrip(uint32L, n))
  }

  test("ulong(n) - roundtrip") { 
    assertEquals(ulong(13).encode(1), Attempt.successful(BitVector.low(13).set(12)))
  }

  test("ulongL(n) - roundtrip") {
    assertEquals(ulongL(13).encode(1), Attempt.successful(BitVector.low(13).set(7)))
  }

  property("support endianess correctly") {
    forAll { (n: Long) =>
      val bigEndian = int64.encode(n).require.toByteVector
      val littleEndian = int64L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(uint32.encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 32-bit unsigned integer")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(uint32.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(32, 8)))
  }
}
