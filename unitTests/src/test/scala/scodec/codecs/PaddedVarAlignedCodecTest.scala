package scodec
package codecs

import scodec.bits._

class PaddedVarAlignedCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "ab")
    roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "abcde")
    roundtrip(paddedVarAlignedBytes(uint16, ascii, 8), "a")
  }

  test("pad to the correct length") {
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 4)
      .encode("a")
      .require, 0x0161000000)
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 4)
      .encode("aaa")
      .require, 0x0361616100)
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 4)
      .encode("aaaa")
      .require, 0x461616161)
  }

  test("pad on a multiplier") {
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 3)
      .encode("aaa")
      .require, 0x03616161)
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 3)
      .encode("aaaa")
      .require, 0x04616161610000)
  }

  test("ignore padded") {
    assertEquals(paddedVarAlignedBytes(uint8, utf8, 4)
      .decode(0x0161000000)
      .require
      .value, "a")
    assertEquals(paddedVarAlignedBytes(uint8, utf8, 4)
      .decode(0x0361616100)
      .require
      .value, "aaa")
  }
}
