package scodec.codecs

import scodec._

class ConditionalCodecTest extends CodecSuite {

  "conditional codec" should {

    "not evaluate if condition is false" in {
      var called = false

      def retCodec(): Codec[String] = {
        called = true
        ascii
      }

      conditional(false, retCodec).encode(Some(""))
      called should be (false)
    }

    "evaluate if condition is true" in {
      var called = false

      def retCodec(): Codec[String] = {
        called = true
        ascii
      }

      conditional(true, retCodec).encode(Some(""))
      called should be (true)
    }

  }
}
