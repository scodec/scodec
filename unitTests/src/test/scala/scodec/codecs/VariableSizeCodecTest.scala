package scodec
package codecs

import scodec.bits._

class VariableSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(variableSizeBits(uint8, utf8), Seq("", "test"))
    roundtripAll(variableSizeBits(uint8, utf8, 10), Seq("", "test"))
  }

  test("encode size followed by value") {
    assertEquals(variableSizeBytes(uint8, utf8).encode("test"), Attempt.successful(
      BitVector(4, 't', 'e', 's', 't')
    ))
    assertEquals(variableSizeBits(uint8, utf8).encode("test"), Attempt.successful(
      BitVector(32, 't', 'e', 's', 't')
    ))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    assertEquals(variableSizeBytes(uint2, utf8).encode("too long"), Attempt.failure(
      Err.General(
        "failed to encode size of [too long]: 8 is greater than maximum value 3 for 2-bit unsigned integer",
        List("size")
      )
    ))
  }

  test("support padding of size") {
    assertEquals(variableSizeBits(uint8, uint8, 0).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x08)
    ))
    assertEquals(variableSizeBytes(uint8, uint8, 0).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x01)
    ))
    assertEquals(variableSizeBits(uint8, uint8, 1).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x09)
    ))
    assertEquals(variableSizeBytes(uint8, uint8, 1).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x02)
    ))
  }

  test("fail encoding if the wrapped codec returns a bit vector with length not divisible by 8") {
    assertEquals(variableSizeBytes(uint8, bool).encode(true), Attempt.failure(
      Err.General("failed to encode size of [true]: 1 is not evenly divisible by 8", List("size"))
    ))
    assertEquals(variableSizeBytes(uint8, byteAligned(bool)).encode(true), Attempt.successful(
      bin"00000001 10000000"
    ))
  }
}
