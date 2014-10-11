package scodec
package codecs

import scalaz.\/
import scodec.bits._

class BooleanCodecTest extends CodecSuite {

  "the bool codec" should {
    "roundtrip" in { roundtripAll(bool, List(true, false)) }
    "encode correctly" in {
      bool.encode(true) shouldBe \/.right(BitVector.high(1))
      bool.encode(false) shouldBe \/.right(BitVector.low(1))
    }
    "decode correctly" in {
      bool.decode(BitVector.low(1)) shouldBe \/.right((BitVector.empty, false))
      bool.decode(BitVector.high(1)) shouldBe \/.right((BitVector.empty, true))
      bool.decode(BitVector.low(2)) shouldBe \/.right((BitVector.low(1), false))
    }
  }

  "the bool(n) codec" should {
    "roundtrip" in { roundtripAll(bool(8), List(true, false)) }
    "encode correctly" in {
      bool(8).encode(true) shouldBe \/.right(BitVector.high(8))
      bool(8).encode(false) shouldBe \/.right(BitVector.low(8))
    }
    "decode correctly" in {
      bool(8).decode(BitVector.low(8)) shouldBe \/.right((BitVector.empty, false))
      bool(8).decode(BitVector.high(8)) shouldBe \/.right((BitVector.empty, true))
      bool(8).decode(BitVector.low(9)) shouldBe \/.right((BitVector.low(1), false))
      bool(8).decode(bin"10000000") shouldBe \/.right((BitVector.empty, true))
      bool(8).decode(bin"00000001") shouldBe \/.right((BitVector.empty, true))
    }
    "return an error when decoding with too few bits" in {
      bool(8).decode(BitVector.low(4)) shouldBe \/.left(Err.insufficientBits(8, 4))
    }
  }
}

