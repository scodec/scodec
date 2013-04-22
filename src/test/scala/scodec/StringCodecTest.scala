package scodec

import scalaz.syntax.id._

import Codecs._


class StringCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(ascii, Seq("test", "", "with\ttabs"))
    roundtripAll(utf8, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("encoding string with chars unsupported by charset results in error") {
    ascii.encode("λ") shouldBe "US-ASCII cannot encode character 'λ'".left
    ascii.encode("Includes a λ") shouldBe "US-ASCII cannot encode character 'λ'".left
  }

  test("decoding buffer with chars unsupported by charset results in error") {
    utf8.decode(BitVector(Bytes.fromValidHexadecimal("0xf4ffffff"))) shouldBe "UTF-8 cannot decode string from '0xf4ffffff'".left
  }
}
