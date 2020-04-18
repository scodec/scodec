package scodec
package codecs

import scodec.bits.BitVector

class VariableSizePrefixedCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(variableSizePrefixedBits(uint8, int32, utf8), Seq(2 -> "", 3 -> "test"))
    roundtripAll(variableSizePrefixedBits(uint8, int32, utf8, 10), Seq(2 -> "", 3 -> "test"))
  }

  test("encode size followed by value") {
    assertEquals(variableSizePrefixedBytes(uint8, int32, utf8).encode(2 -> "test"), Attempt.successful(
      BitVector(4, 0, 0, 0, 2, 't', 'e', 's', 't')
    ))
    assertEquals(variableSizePrefixedBits(uint8, int32, utf8).encode(2 -> "test"), Attempt.successful(
      BitVector(32, 0, 0, 0, 2, 't', 'e', 's', 't')
    ))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    assertEquals(variableSizePrefixedBytes(uint2, int32, utf8).encode(2 -> "too long"), Attempt
      .failure(
        Err(
          "[too long] is too long to be encoded: 8 is greater than maximum value 3 for 2-bit unsigned integer"
        )
      ))
  }

  test("support padding of size") {
    assertEquals(variableSizePrefixedBits(uint8, int32, uint8, 0).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x08)))
    assertEquals(variableSizePrefixedBytes(uint8, int32, uint8, 0).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x01)))

    assertEquals(variableSizePrefixedBits(uint8, int32, uint8, 1).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x09)))
    assertEquals(variableSizePrefixedBytes(uint8, int32, uint8, 1).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x02)))
  }
}
