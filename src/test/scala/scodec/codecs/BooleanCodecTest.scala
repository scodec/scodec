package scodec
package codecs

import scalaz.syntax.id._
import scodec.bits._

class BooleanCodecTest extends CodecSuite {

  test("1-bit encode") {
    bool.encode(true) shouldBe BitVector.high(1).right
    bool.encode(false) shouldBe BitVector.low(1).right
  }

  test("1-bit decode") {
    bool.decode(BitVector.low(1)) shouldBe (BitVector.empty, false).right
    bool.decode(BitVector.high(1)) shouldBe (BitVector.empty, true).right
    bool.decode(BitVector.low(2)) shouldBe (BitVector.low(1), false).right
  }

  test("n-bit encode") {
    bool(8).encode(true) shouldBe BitVector.high(8).right
    bool(8).encode(false) shouldBe BitVector.low(8).right
  }

  test("n-bit decode") {
    bool(8).decode(BitVector.low(8)) shouldBe (BitVector.empty, false).right
    bool(8).decode(BitVector.high(8)) shouldBe (BitVector.empty, true).right
    bool(8).decode(BitVector.low(9)) shouldBe (BitVector.low(1), false).right
    bool(8).decode(bin"10000000") shouldBe (BitVector.empty, true).right
    bool(8).decode(bin"00000001") shouldBe (BitVector.empty, true).right
  }

  test("n-bit decode - decoding with too few bits") {
    bool(8).decode(BitVector.low(4)) shouldBe ("cannot acquire 8 bits from a vector that contains 4 bits".left)
  }
}

