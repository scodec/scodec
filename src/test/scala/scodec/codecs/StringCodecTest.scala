package scodec
package codecs

import scalaz.syntax.either._
import scodec.bits.{ BitVector, ByteVector }

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
    utf8.decode(BitVector(ByteVector.fromValidHex("0xf4ffffff"))) shouldBe "UTF-8 cannot decode string from '0xf4ffffff'".left
  }
}
