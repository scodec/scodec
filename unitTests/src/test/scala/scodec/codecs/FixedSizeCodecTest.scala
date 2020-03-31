package scodec
package codecs

import scodec.bits._

class FixedSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(fixedSizeBits(32, utf8), "test")
    roundtrip(fixedSizeBits(8, uint8), 12)
    roundtrip(fixedSizeBits(16, uint8), 12)
  }

  test("pad appropriately") {
    assertEquals(fixedSizeBits(16, uint8).encode(12).require, BitVector(hex"0c00"))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    val encoded = utf8.encode("test").require
    assertEquals(fixedSizeBits(32, utf8).decode(encoded ++ BitVector.low(48)), Attempt.successful(
      DecodeResult("test", BitVector.low(48))
    ))
    assertEquals(fixedSizeBits(24, utf8).encode("test"), Attempt.failure(
      Err("[test] requires 32 bits but field is fixed size of 24 bits")
    ))
  }
}
