package scodec
package codecs

import scodec.bits.{ BitVector, ByteVector }

class StringCodecTest extends CodecSuite {

  "ascii codec" should {
    "roundtrip" in {
      roundtripAll(ascii, Seq("test", "", "with\ttabs"))
    }
  }

  "ascii32 codec" should {
    "roundtrip" in {
      roundtripAll(ascii32, Seq("test", "", "with\ttabs"))
    }
  }

  "ascii32L codec" should {
    "roundtrip" in {
      roundtripAll(ascii32L, Seq("test", "", "with\ttabs"))
    }
  }

  "utf8 codec" should {
    "roundtrip" in {
      roundtripAll(utf8, Seq("test", "", "with\ttabs", "withλ"))
    }
  }

  "utf8_32 codec" should {
    "roundtrip" in {
      roundtripAll(utf8_32, Seq("test", "", "with\ttabs", "withλ"))
    }
  }

  "utf8_32L codec" should {
    "roundtrip" in {
      roundtripAll(utf8_32L, Seq("test", "", "with\ttabs", "withλ"))
    }
  }

  "string codecs" should {
    "fail encoding with an error when string to encode contains chars unsupported by charset" in {
      ascii.encode("λ") shouldBe Attempt.failure(Err("US-ASCII cannot encode character 'λ'"))
      ascii.encode("Includes a λ") shouldBe Attempt.failure(Err("US-ASCII cannot encode character 'λ'"))
    }

    "fail decoding with an error when buffer contains bytes unsupported by charset" in {
      utf8.decode(BitVector(ByteVector.fromValidHex("0xf4ffffff"))) shouldBe Attempt.failure(Err("UTF-8 cannot decode string from '0xf4ffffff'"))
    }
  }
}
