package scodec
package codecs

import scodec.bits._

class BytesCodecTest extends CodecSuite {

  test("roundtrip when input size match") { roundtrip(bytes(8), hex"0102030405060708") }

  test("pad with zero when encoding with less bytes than expected") {
    assertEquals(bytes(8).encode(hex"0101010101").require, BitVector(hex"0101010101000000"))
  }

  test("return an error when encoding with more bytes than expected") {
    assertEquals(bytes(8).encode(hex"010203040506070809"), Attempt.failure(
      Err(
        "[BitVector(72 bits, 0x010203040506070809)] requires 72 bits but field is fixed size of 64 bits"
      )
    ))
  }
}
