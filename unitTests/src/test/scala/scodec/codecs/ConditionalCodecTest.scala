package scodec.codecs

import scodec._
import scodec.bits._

class ConditionalCodecTest extends CodecSuite {

  "conditional codec" should {

    "not evaluate if condition is false when encoding" in {
      var called = false

      def retCodec(): Codec[String] = {
        called = true
        ascii
      }

      val result = conditional(false, retCodec).encode(Some("00"))

      called should be (false)
      result shouldBe a [Attempt.Successful[_]]
      result.require shouldBe BitVector.empty
    }

    "evaluate if condition is true when encoding" in {
      var called = false

      def retCodec(): Codec[String] = {
        called = true
        ascii
      }

      val result = conditional(true, retCodec).encode(Some("00"))

      called should be (true)
      result shouldBe a [Attempt.Successful[_]]
      result.require shouldBe hex"3030".bits
    }

    "not evaluate if condition is false when decoding" in {
      var called = false

      def retCodec(): Codec[String] = {
        called = true
        ascii
      }

      val result = conditional(false, retCodec).decode(hex"3030".bits)

      called should be (false)
      result shouldBe a [Attempt.Successful[_]]
      result.require.value shouldBe None
    }

    "evaluate if condition is true when decoding" in {
      var called = false

      def retCodec(): Codec[String] = {
        called = true
        ascii
      }

      val result = conditional(true, retCodec).decode(hex"3030".bits)

      called should be (true)
      result shouldBe a [Attempt.Successful[_]]
      result.require.value shouldBe Some("00")
    }

  }
}
