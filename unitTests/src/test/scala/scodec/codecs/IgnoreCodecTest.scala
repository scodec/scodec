package scodec
package codecs

import scodec.bits.BitVector

class IgnoreCodecTest extends CodecSuite {

  // The scalatest ignore method shadows this
  def ign(size: Int) = scodec.codecs.ignore(size.toLong)

  test("roundtrip") {
    val codec = ign(2) ~> uint4 <~ ign(2)
    assertEquals(codec.decode(BitVector(0xff)), Attempt.successful(DecodeResult(15, BitVector.empty)))
    assertEquals(codec.encode(15), Attempt.successful(BitVector(0x3c)))
  }
}
