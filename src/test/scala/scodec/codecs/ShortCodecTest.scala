package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.option._
import org.scalacheck.Gen
import scodec.bits.BitVector

class ShortCodecTest extends CodecSuite {
  def check(low: Short, high: Short)(f: (Short) => Unit) {
    forAll(Gen.choose(low, high)) { n =>
      whenever(n >= low) { f(n) }
    }
  }

  "the short16 codec" should { "roundtrip" in { forAll { (n: Short) => roundtrip(short16, n) } } }
  "the short16L codec" should { "roundtrip" in { forAll { (n: Short) => roundtrip(short16L, n) } } }
  "the ushort(n) codec" should { "roundtrip" in { forAll { (n: Short) => whenever(n >= 0) { roundtrip(ushort(15), n) } } } }
  "the ushortL(n) codec" should { "roundtrip" in { forAll { (n: Short) => whenever(n >= 0) { roundtrip(ushortL(15), n) } } } }

  "the short codecs" should {
    "support endianess correctly" in {
      forAll { (n: Short) =>
        val bigEndian = short16.encode(n).toOption.err("big").toByteVector
        val littleEndian = short16L.encode(n).toOption.err("little").toByteVector
        littleEndian shouldBe bigEndian.reverse
      }
      check(0, 8191) { (n: Short) =>
        whenever(n >= 0 && n <= 8191) {
          val bigEndian = ushort(13).encodeValid(n)
          val littleEndian = ushortL(13).encodeValid(n).toByteVector
          val flipped = BitVector(littleEndian.last).take(5) ++ littleEndian.init.reverse.toBitVector
          flipped shouldBe bigEndian
        }
      }
    }

    "return an error when value to encode is out of legal range" in {
      short(15).encode(Short.MaxValue) shouldBe \/.left(Err("32767 is greater than maximum value 16383 for 15-bit signed short"))
      short(15).encode(Short.MinValue) shouldBe \/.left(Err("-32768 is less than minimum value -16384 for 15-bit signed short"))
      ushort(15).encode(-1) shouldBe \/.left(Err("-1 is less than minimum value 0 for 15-bit unsigned short"))
    }

    "return an error when decoding with too few bits" in {
      short16.decode(BitVector.low(8)) shouldBe \/.left(Err.insufficientBits(16, 8))
    }
  }
}
