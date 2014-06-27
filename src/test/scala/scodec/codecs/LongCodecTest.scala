package scodec
package codecs

import scalaz.syntax.id._
import org.scalacheck.Gen
import scodec.bits.BitVector

class LongCodecTest extends CodecSuite {
  def check(low: Long, high: Long)(f: (Long) => Unit) {
    forAll(Gen.choose(low, high)) { n =>
      whenever(n >= low) { f(n) }
    }
  }

  test("int64") { forAll { (n: Long) => roundtrip(int64, n) } }
  test("int64L") { forAll { (n: Long) => roundtrip(int64L, n) } }
  test("uint32") { check(0, 1L << (32 - 1)) { (n: Long) => roundtrip(uint32, n) } }
  test("uint32L") { check(0L, (1L << 32) - 1) { (n: Long) => roundtrip(uint32L, n) } }

  test("ulong(13)") { ulong(13).encode(1) shouldBe BitVector.low(13).set(12).right }
  test("ulongL(13)") { ulongL(13).encode(1) shouldBe BitVector.low(13).set(7).right }


  test("endianess") {
    forAll { (n: Long) =>
      val bigEndian = int64.encode(n).toOption.get.toByteVector
      val littleEndian = int64L.encode(n).toOption.get.toByteVector
      littleEndian shouldBe bigEndian.reverse
    }
  }

  test("range checking") {
    uint32.encode(-1) shouldBe "-1 is less than minimum value 0 for 32-bit unsigned integer".left
  }

  test("decoding with too few bits") {
    uint32.decode(BitVector.low(8)) shouldBe ("cannot acquire 32 bits from a vector that contains 8 bits".left)
  }
}
