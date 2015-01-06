package scodec
package codecs

import scodec.bits.BitVector

class IgnoreCodecTest extends CodecSuite {

  // The scalatest ignore method shadows this
  def ign(size: Int) = scodec.codecs.ignore(size)

  "the ignore codec" should {
    "roundtrip" in {
      val codec = ign(2) ~> uint4 <~ ign(2)
      codec.decode(BitVector(0xff)) shouldBe DecodeResult.successful(15, BitVector.empty)
      codec.encode(15) shouldBe EncodeResult.successful(BitVector(0x3c))
    }
  }
}
