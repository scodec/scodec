package scodec
package codecs

import scalaz.\/-

import scodec.bits._

class ConstantCodecTest extends CodecSuite {

  "the constant codec" should {
    "fail to decode when codec does not match" in {
      constant(1).decode(hex"02".bits) shouldBe 'left
    }
  }

  "the constantLenient codec" should {
    "not fail to decode when codec does not match" in {
      constantLenient(1).decode(hex"02".bits) shouldBe \/-((BitVector.empty, ()))
    }
  }
}
