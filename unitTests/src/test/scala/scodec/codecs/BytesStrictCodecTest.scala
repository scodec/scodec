package scodec
package codecs

import scodec.bits._

class BytesStrictCodecTest extends CodecSuite {

  test("roundtrip when input size is correct") { roundtrip(bytesStrict(8), hex"0102030405060708") }

  test("return an error when encoding with less bytes than expected") {
    assertEquals(bytesStrict(8).encode(hex"0102030405"), Attempt.failure(
      Err(
        "[BitVector(40 bits, 0x0102030405)] requires 40 bits but field is fixed size of exactly 64 bits"
      )
    ))
  }

  test("return an error when encoding with more bytes than expected") {
    assertEquals(bytesStrict(8).encode(hex"010203040506070809"), Attempt.failure(
      Err(
        "[BitVector(72 bits, 0x010203040506070809)] requires 72 bits but field is fixed size of exactly 64 bits"
      )
    ))
  }

  test("return an error when decoding with less bytes than expected") {
    assertEquals(bytesStrict(8).decode(BitVector(hex"0102030405")), Attempt.failure(
      Err("expected exactly 64 bits but got 40 bits")
    ))
  }

  test("return an error when decoding with more bytes than expected") {
    assertEquals(bytesStrict(8).decode(BitVector(hex"010203040506070809")), Attempt.failure(
      Err("expected exactly 64 bits but got 72 bits")
    ))
  }
}
