package scodec
package codecs

import scalaz.{\/-, -\/}
import scodec.bits.BitVector

class VariableSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(variableSizeBits(uint8, ascii), Seq("", "test"))
    roundtripAll(variableSizeBits(uint8, ascii, 10), Seq("", "test"))
  }

  test("successful encoding") {
    variableSizeBytes(uint8, ascii).encode("test") shouldBe \/-(BitVector(4, 't', 'e', 's', 't'))
    variableSizeBits(uint8, ascii).encode("test") shouldBe \/-(BitVector(32, 't', 'e', 's', 't'))
  }

  test("range checking") {
    variableSizeBytes(uint2, ascii).encode("too long") shouldBe -\/("[too long] is too long to be encoded: 8 is greater than maximum value 3 for 2-bit unsigned integer")
  }

  test("size padding encoding") {
    variableSizeBits(uint8, uint8, 0).encode(0).map { _.take(8) } shouldBe \/-(BitVector(0x08))
    variableSizeBytes(uint8, uint8, 0).encode(0).map { _.take(8) } shouldBe \/-(BitVector(0x01))

    variableSizeBits(uint8, uint8, 1).encode(0).map { _.take(8) } shouldBe \/-(BitVector(0x09))
    variableSizeBytes(uint8, uint8, 1).encode(0).map { _.take(8) } shouldBe \/-(BitVector(0x02))
  }
}
