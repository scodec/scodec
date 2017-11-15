package scodec
package codecs

import scodec.bits.HexStringSyntax

class PaddedVarAlignedCodecTest extends CodecSuite {

  "paddedVarAlignedCodec" should {
    "roundtrip" in {
      roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "ab")
      roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "abcde")
      roundtrip(paddedVarAlignedBytes(uint16, ascii, 8), "a")
    }

    "pad to the correct length" in {
      paddedVarAlignedBytes(uint8, utf8, 4).encode("a").require shouldBe hex"0x0161000000".toBitVector 
      paddedVarAlignedBytes(uint8, utf8, 4).encode("aaa").require shouldBe hex"0x0361616100".toBitVector 
      paddedVarAlignedBytes(uint8, utf8, 4).encode("aaaa").require shouldBe hex"0x461616161".toBitVector
    }

    "pad on a multiplier" in {
      paddedVarAlignedBytes(uint8, utf8, 3).encode("aaa").require shouldBe hex"0x03616161".toBitVector 
      paddedVarAlignedBytes(uint8, utf8, 3).encode("aaaa").require shouldBe hex"0x04616161610000".toBitVector
    }
   
    "ignore padded" in {
      paddedVarAlignedBytes(uint8, utf8, 4).decode(hex"0x0161000000".toBitVector).require.value shouldBe "a" 
      paddedVarAlignedBytes(uint8, utf8, 4).decode(hex"0x0361616100".toBitVector).require.value shouldBe "aaa" 

    }
  }
}
