package scodec
package codecs

import scodec.bits._

class ByteAlignedCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(byteAligned(int32), Int.MaxValue)
    roundtrip(byteAligned(uint(6)), 2)
  }

  test("pad appropriately") {
    assertEquals(byteAligned(uint(6)).encode(1).require, bin"00000100")
    assertEquals(byteAligned(listOfN(uint(4), uint8))
      .encode(List(1, 2, 3))
      .require, bin"00110000000100000010000000110000")
  }

  test("de-pad appropriately") {
    assertEquals(byteAligned(listOfN(uint(4), uint8))
      .decode(bin"001100000001000000100000001100001111")
      .require, DecodeResult(List(1, 2, 3), bin"1111"))
  }

  test("compute size bounds appropriately") {
    assertEquals(byteAligned(listOfN(uint(4), uint8)).sizeBound, SizeBound.atLeast(8))
  }
}
