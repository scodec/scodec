package scodec
package codecs

import scalaz.\/-
import scalaz.std.anyVal.unitInstance

class IgnoreCodecTest extends CodecSuite {

  // The scalatest ignore method shadoes this
  def ign(size: Int) = scodec.codecs.ignore(size)

  test("roundtrip") {
    val codec = ign(2) ~> uint4 <~ ign(2)
    codec.decode(BitVector(0xff)) shouldBe \/-((BitVector.empty, 15))
    codec.encode(15) shouldBe \/-(BitVector(0x3c))
  }
}
