package scodec
package codecs

import scodec.bits._

class MapCodecTest extends CodecSuite {

  "the mapOfN codec" should {

    "limit decoding to the specified number of records" in {
      val codec = mapOfN(provide(3), ascii32, uint8)
      codec.decode(hex"00000001610a00000001620b00000001630c00000001640d".bits) shouldBe Attempt.successful(DecodeResult(Map("a" -> 10, "b" -> 11, "c" -> 12), hex"00000001640d".bits))
    }

    "support encoding size before vector contents" in {
      val codec = mapOfN(int32, ascii32, uint8)
      codec.encode(Map("a" -> 10, "b" -> 11, "c" -> 12, "d" -> 13)) shouldBe Attempt.successful(hex"0000000400000001610a00000001620b00000001630c00000001640d".bits)
    }

    "support not encoding size before vector contents" in {
      val codec = mapOfN(provide(4), ascii32, uint8)
      codec.encode(Map("a" -> 10, "b" -> 11, "c" -> 12, "d" -> 13)) shouldBe Attempt.successful(hex"00000001610a00000001620b00000001630c00000001640d".bits)
    }

    "fails decoding if < N elements decoded" in {
      val codec = mapOfN(provide(10), ascii32, uint8)
      codec.decode(hex"00000001610a00000001620b00000001630c00000001640d".bits) shouldBe Attempt.failure(Err("Insufficient number of elements: decoded 4 but should have decoded 10"))
    }
  }
}
