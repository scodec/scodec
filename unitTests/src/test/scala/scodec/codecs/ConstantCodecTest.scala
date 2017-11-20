package scodec
package codecs

import scodec.bits._

class ConstantCodecTest extends CodecSuite {

  "the constant codec" should {
    "fail to decode when codec does not match" in {
      constant(1).decode(hex"02".bits) shouldBe Attempt.failure(Err("expected constant BitVector(8 bits, 0x01) but got BitVector(8 bits, 0x02)"))
    }
  }

  "the constantLenient codec" should {
    "not fail to decode when codec does not match" in {
      constantLenient(1).decode(hex"02".bits) shouldBe Attempt.successful(DecodeResult((), BitVector.empty))
    }
  }
}
