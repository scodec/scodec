package scodec
package codecs

import scodec.bits._

class PaddedFixedSizeCodecTest extends CodecSuite {
  val ones = constant(hex"ff")

  test("paddedFixedSizeBytes - roundtrip") {
    roundtrip(paddedFixedSizeBytes(4, utf8, ones), "test")
    roundtrip(paddedFixedSizeBytes(1, uint8, ones), 12)
    roundtrip(paddedFixedSizeBytes(2, uint8, ones), 12)
  }

  test("paddedFixedSizeBytes - pad appropriately on encode") {
    assertEquals(paddedFixedSizeBytes(1, uint8, ones).encode(12).require, BitVector(hex"0c"))
    assertEquals(paddedFixedSizeBytes(2, uint8, ones).encode(12).require, BitVector(hex"0cff"))
    assertEquals(paddedFixedSizeBytes(3, uint8, ones).encode(12).require, BitVector(hex"0cffff"))
  }

  test("paddedFixedSizeBytes - failed padCodec encode only reported if used") {
    assertBitsEqual(paddedFixedSizeBytes(1, uint8, codecs.fail(Err("bad pad")))
      .encode(12)
      .require, 0x0c)
  }

  test("paddedFixedSizeBytes - report failed padCodec encode") {
    assertEquals(paddedFixedSizeBytes(2, uint8, codecs.fail(Err("bad pad"))).encode(12), Attempt
      .failure(Err("bad pad").pushContext("padding")))
  }

  test("paddedFixedSizeBytes - support pkcs5/pkcs7 style padding") {
    val codec = paddedFixedSizeBytesDependent(4, uint8, n => constant(n / 8))
    assertBitsEqual(codec.encode(0).require, 0x00030303)
    roundtripAll(codec, Seq(0, 1, 255))
    assertEquals(codec.decode(0x00040404), Attempt.failure(
      Err("expected constant BitVector(8 bits, 0x03) but got BitVector(8 bits, 0x04)")
        .pushContext("padding")
    ))
  }

  test("paddedFixedSizeBits - roundtrip") {
    roundtrip(paddedFixedSizeBits(32, utf8, ones), "test")
    roundtrip(paddedFixedSizeBits(8, uint8, ones), 12)
    roundtrip(paddedFixedSizeBits(16, uint8, ones), 12)
  }

  test("paddedFixedSizeBits - pad appropriately on encode") {
    assertBitsEqual(paddedFixedSizeBits(8, uint8, ones).encode(12).require, 0x0c)
    assertBitsEqual(paddedFixedSizeBits(16, uint8, ones).encode(12).require, 0x0cff)
    assertBitsEqual(paddedFixedSizeBits(24, uint8, ones).encode(12).require, 0x0cffff)
  }

  test("paddedFixedSizeBits - encode fails if padding does not exactly fill") {
    assertEquals(paddedFixedSizeBits(13, uint8, ones).encode(12), Attempt.failure(
      Err("padding overflows fixed size field of 13 bits").pushContext("padding")
    ))
    assertEquals(paddedFixedSizeBits(27, uint8, ones).encode(12), Attempt.failure(
      Err("padding overflows fixed size field of 27 bits").pushContext("padding")
    ))
  }

  test("paddedFixedSizeBits - decode validate remainder") {
    assertEquals(paddedFixedSizeBits(8, uint8, ones).decode(0x0c).require.value, 12)
    assertEquals(paddedFixedSizeBits(24, uint8, ones).decode(0x0cffff).require.value, 12)
  }

  test("paddedFixedSizeBits - decode ignores remainder") {
    assertEquals(paddedFixedSizeBits(24, uint8, constantLenient(hex"ff"))
      .decode(0x0c0000)
      .require
      .value, 12)
    assertEquals(paddedFixedSizeBits(11, uint8, constantLenient(hex"ff"))
      .decode(0x0c0000)
      .require
      .value, 12)
  }

  test("paddedFixedSizeBits - decode fails if remaining bits do not match pad") {
    assertEquals(paddedFixedSizeBits(8, uint8, ones).decode(0x0c).require.value, 12)
    assertEquals(paddedFixedSizeBits(9, uint8, ones).decode(0x0c00), Attempt.failure(
      Err.insufficientBits(8, 1).pushContext("padding")
    ))
    assertEquals(paddedFixedSizeBits(16, uint8, ones).decode(0x0c00), Attempt.failure(
      Err("expected constant BitVector(8 bits, 0xff) but got BitVector(8 bits, 0x00)")
        .pushContext("padding")
    ))
    assertEquals(paddedFixedSizeBits(24, uint8, ones).decode(0x0cff11), Attempt.failure(
      Err("expected constant BitVector(8 bits, 0xff) but got BitVector(8 bits, 0x11)")
        .pushContext("padding")
    ))
  }

  test("paddedFixedSizeBits - fail encoding when value is too large to be encoded by size codec") {
    val encoded = utf8.encode("test").require
    assertEquals(paddedFixedSizeBits(32, utf8, ones).decode(encoded ++ BitVector.low(48)), Attempt
      .successful(DecodeResult("test", BitVector.low(48))))
    assertEquals(paddedFixedSizeBits(24, utf8, ones).encode("test"), Attempt.failure(
      Err("[test] requires 32 bits but field is fixed size of 24 bits")
    ))
  }

  test("paddedFixedSizeBits - support pkcs5/pkcs7 style padding") {
    assertBitsEqual(paddedFixedSizeBitsDependent(32, uint8, n => constant(n))
      .encode(0)
      .require, 0x00181818)
  }
}
