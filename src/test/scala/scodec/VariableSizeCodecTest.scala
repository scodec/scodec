package scodec

import scalaz.{\/-, -\/}

import Codecs._


class VariableSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(variableSizeBits(uint8, ascii), Seq("", "test"))
  }

  test("successful encoding") {
    variableSizeBytes(uint8, ascii).encode("test") shouldBe \/-(BitVector(4, 't', 'e', 's', 't'))
    variableSizeBits(uint8, ascii).encode("test") shouldBe \/-(BitVector(32, 't', 'e', 's', 't'))
  }

  test("range checking") {
    variableSizeBytes(uint2, ascii).encode("too long") shouldBe -\/("[too long] is too long to be encoded: 8 is greater than maximum value 3 for 2-bit unsigned integer")
  }
}
