package scodec
package codecs

import scodec.bits._
import scodec.codecs._

class ListCodecTest extends CodecSuite {

  "the listOfN codec" should {

    "limit decoding to the specified number of records" in {
      val codec = listOfN(provide(10), uint8)
      val buffer = BitVector.low(8 * 100)
      codec.decode(buffer) shouldBe Attempt.successful(DecodeResult(List.fill(10)(0), BitVector.low(8 * 90)))
    }

    "support encoding size before vector contents" in {
      val codec = listOfN(int32, uint8)
      codec.encode((1 to 10).toList) shouldBe Attempt.successful(hex"0000000a0102030405060708090a".bits)
    }

    "support not encoding size before vector contents" in {
      val codec = listOfN(provide(10), uint8)
      codec.encode((1 to 10).toList) shouldBe Attempt.successful(hex"102030405060708090a".bits)
    }
  }
}
