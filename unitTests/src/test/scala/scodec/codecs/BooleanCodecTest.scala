package scodec
package codecs

import scodec.bits._

class BooleanCodecTest extends CodecSuite {

  "the bool codec" should {
    "roundtrip" in { roundtripAll(bool, List(true, false)) }
    "encode correctly" in {
      bool.encode(true) shouldBe Attempt.successful(BitVector.high(1))
      bool.encode(false) shouldBe Attempt.successful(BitVector.low(1))
    }
    "decode correctly" in {
      bool.decode(BitVector.low(1)) shouldBe Attempt.successful(
        DecodeResult(false, BitVector.empty)
      )
      bool.decode(BitVector.high(1)) shouldBe Attempt.successful(
        DecodeResult(true, BitVector.empty)
      )
      bool.decode(BitVector.low(2)) shouldBe Attempt.successful(
        DecodeResult(false, BitVector.low(1))
      )
    }
  }

  "the bool(n) codec" should {
    "roundtrip" in { roundtripAll(bool(8), List(true, false)) }
    "encode correctly" in {
      bool(8).encode(true) shouldBe Attempt.successful(BitVector.high(8))
      bool(8).encode(false) shouldBe Attempt.successful(BitVector.low(8))
    }
    "decode correctly" in {
      bool(8).decode(BitVector.low(8)) shouldBe Attempt.successful(
        DecodeResult(false, BitVector.empty)
      )
      bool(8).decode(BitVector.high(8)) shouldBe Attempt.successful(
        DecodeResult(true, BitVector.empty)
      )
      bool(8).decode(BitVector.low(9)) shouldBe Attempt.successful(
        DecodeResult(false, BitVector.low(1))
      )
      bool(8).decode(bin"10000000") shouldBe Attempt.successful(DecodeResult(true, BitVector.empty))
      bool(8).decode(bin"00000001") shouldBe Attempt.successful(DecodeResult(true, BitVector.empty))
    }
    "return an error when decoding with too few bits" in {
      bool(8).decode(BitVector.low(4)) shouldBe Attempt.failure(Err.insufficientBits(8, 4))
    }
  }
}
