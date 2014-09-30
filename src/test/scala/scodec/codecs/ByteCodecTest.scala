package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.option._
import org.scalacheck.Gen
import scodec.bits.BitVector

class ByteCodecTest extends CodecSuite {
  def check(low: Byte, high: Byte)(f: (Byte) => Unit) {
    forAll(Gen.choose(low, high)) { n =>
      whenever(n >= low) { f(n) }
    }
  }

  "the byte codec" should { "roundtrip" in { forAll { (n: Byte) => roundtrip(byte, n) } } }
  "the ubyte(n) codec" should { "roundtrip" in { forAll { (n: Byte) => whenever(n >= 0) { roundtrip(ubyte(7), n) } } } }

  "the byte codecs" should {
    "return an error when value to encode is out of legal range" in {
      byte(7).encode(Byte.MaxValue) shouldBe \/.left("127 is greater than maximum value 63 for 7-bit signed byte")
      byte(7).encode(Byte.MinValue) shouldBe \/.left("-128 is less than minimum value -64 for 7-bit signed byte")
      ubyte(7).encode(-1) shouldBe \/.left("-1 is less than minimum value 0 for 7-bit unsigned byte")
    }

    "return an error when decoding with too few bits" in {
      byte.decode(BitVector.low(4)) shouldBe \/.left("cannot acquire 8 bits from a vector that contains 4 bits")
    }
  }
}
