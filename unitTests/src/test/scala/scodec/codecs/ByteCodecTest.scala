package scodec
package codecs

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class ByteCodecTest extends CodecSuite {
  def check(low: Byte, high: Byte)(f: (Byte) => Unit): Unit =
    forAll(Gen.choose(low, high))(n => f(n))

  property("byte - roundtrip") {
    forAll((n: Byte) => roundtrip(byte, n))
  }

  property("ubyte(n) - roundtrip") {
    forAll(Gen.choose(0, 127))(n => roundtrip(ubyte(7), n.toByte))
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(byte(7).encode(Byte.MaxValue), Attempt.failure(
      Err("127 is greater than maximum value 63 for 7-bit signed byte")
    ))
    assertEquals(byte(7).encode(Byte.MinValue), Attempt.failure(
      Err("-128 is less than minimum value -64 for 7-bit signed byte")
    ))
    assertEquals(ubyte(7).encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 7-bit unsigned byte")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(byte.decode(BitVector.low(4)), Attempt.failure(Err.insufficientBits(8, 4)))
  }
}
