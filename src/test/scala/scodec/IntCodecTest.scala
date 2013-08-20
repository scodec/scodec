package scodec

import scalaz.syntax.id._
import org.scalacheck.Gen

import Codecs._


class IntCodecTest extends CodecSuite {
  def check(low: Int, high: Int)(f: (Int) => Unit) {
    forAll(Gen.choose(low, high)) { n =>
      whenever(n >= low) { f(n) }
    }
  }

  test("int32") { forAll { (n: Int) => roundtrip(int32, n) } }
  test("int32L") { forAll { (n: Int) => roundtrip(int32L, n) } }
  test("uint24L") { check(0, (1 << 24) - 1) { (n: Int) => roundtrip(uint24L, n) } }
  test("int16") { check(-32768, 32767) { (n: Int) => roundtrip(int16, n) } }
  test("uint16") { check(0, 65535) { (n: Int) => roundtrip(uint16, n) } }
  test("uint16L") { check(0, 65535) { (n: Int) => roundtrip(uint16L, n) } }
  test("uint8") { check(0, 255) { (n: Int) => roundtrip(uint8, n) } }
  test("uint8L") { check(0, 255) { (n: Int) => roundtrip(uint8L, n) } }
  test("uint4") { check(0, 1 << 3) { (n: Int) => roundtrip(uint4, n) } }
  test("uint4L") { check(0, (1 << 4) - 1) { (n: Int) => roundtrip(uint4L, n) } }

  test("endianess") {
    forAll { (n: Int) =>
      val bigEndian = int32.encode(n).toOption.get.toByteVector
      val littleEndian = int32L.encode(n).toOption.get.toByteVector
      littleEndian shouldBe bigEndian.reverse
    }
    forAll(Gen.choose(0, 15)) { (n: Int) =>
      val bigEndian = uint4.encode(n).toOption.get.toByteVector
      val littleEndian = uint4L.encode(n).toOption.get.toByteVector
      littleEndian shouldBe bigEndian.reverse
    }
  }

  test("range checking") {
    int16.encode(65536) shouldBe "65536 is greater than maximum value 32767 for 16-bit signed integer".left
    int16.encode(-32769) shouldBe "-32769 is less than minimum value -32768 for 16-bit signed integer".left
    uint16.encode(-1) shouldBe "-1 is less than minimum value 0 for 16-bit unsigned integer".left
  }

  test("decoding with too few bits") {
    int16.decode(BitVector.low(8)) shouldBe ("cannot acquire 16 bits from a vector that contains 8 bits".left)
  }
}
