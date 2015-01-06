package scodec
package codecs

import scodec.bits._

class BooleanCodecTest extends CodecSuite {

  "the bool codec" should {
    "roundtrip" in { roundtripAll(bool, List(true, false)) }
    "encode correctly" in {
      bool.encode(true) shouldBe EncodeResult.successful(BitVector.high(1))
      bool.encode(false) shouldBe EncodeResult.successful(BitVector.low(1))
    }
    "decode correctly" in {
      bool.decode(BitVector.low(1)) shouldBe DecodeResult.successful(false, BitVector.empty)
      bool.decode(BitVector.high(1)) shouldBe DecodeResult.successful(true, BitVector.empty)
      bool.decode(BitVector.low(2)) shouldBe DecodeResult.successful(false, BitVector.low(1))
    }
  }

  "the bool(n) codec" should {
    "roundtrip" in { roundtripAll(bool(8), List(true, false)) }
    "encode correctly" in {
      bool(8).encode(true) shouldBe EncodeResult.successful(BitVector.high(8))
      bool(8).encode(false) shouldBe EncodeResult.successful(BitVector.low(8))
    }
    "decode correctly" in {
      bool(8).decode(BitVector.low(8)) shouldBe DecodeResult.successful(false, BitVector.empty)
      bool(8).decode(BitVector.high(8)) shouldBe DecodeResult.successful(true, BitVector.empty)
      bool(8).decode(BitVector.low(9)) shouldBe DecodeResult.successful(false, BitVector.low(1))
      bool(8).decode(bin"10000000") shouldBe DecodeResult.successful(true, BitVector.empty)
      bool(8).decode(bin"00000001") shouldBe DecodeResult.successful(true, BitVector.empty)
    }
    "return an error when decoding with too few bits" in {
      bool(8).decode(BitVector.low(4)) shouldBe DecodeResult.failure(Err.insufficientBits(8, 4))
    }
  }
}

