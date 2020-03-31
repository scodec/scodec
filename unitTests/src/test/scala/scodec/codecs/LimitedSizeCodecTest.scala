package scodec
package codecs

import scodec.bits._

class LimitedSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(limitedSizeBits(32, utf8), "test")
    roundtrip(limitedSizeBits(8, uint8), 12)
    roundtrip(limitedSizeBits(16, uint8), 12)
  }

  test("not pad") {
    assertEquals(limitedSizeBits(16, uint8).encode(12).require, BitVector(hex"0c"))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    val encoded = utf8.encode("test").require
    assertEquals(limitedSizeBits(32, utf8).decode(encoded ++ BitVector.low(48)), Attempt.successful(
      DecodeResult("test", BitVector.low(48))
    ))
    assertEquals(limitedSizeBits(24, utf8).encode("test"), Attempt.failure(
      Err("[test] requires 32 bits but field is limited to 24 bits")
    ))
  }
}
