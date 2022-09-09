package scodec
package codecs

import scodec.bits._

class ByteAlignedCodecTest extends CodecSuite {

  "the byteAligned combinator" should {
    "roundtrip" in {
      roundtrip(byteAligned(int32), Int.MaxValue)
      roundtrip(byteAligned(uint(6)), 2)
    }

    "pad appropriately" in {
      byteAligned(uint(6)).encode(1).require shouldBe bin"00000100"
      byteAligned(listOfN(uint(4), uint8))
        .encode(List(1, 2, 3))
        .require shouldBe bin"00110000000100000010000000110000"
    }

    "de-pad appropriately" in {
      byteAligned(listOfN(uint(4), uint8))
        .decode(bin"001100000001000000100000001100001111")
        .require shouldBe DecodeResult(List(1, 2, 3), bin"1111")
    }

    "compute size bounds appropriately" in {
      byteAligned(listOfN(uint(4), uint8)).sizeBound shouldBe SizeBound.atLeast(8)
    }
  }
}
