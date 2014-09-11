package scodec

import scodec.bits._
import scodec.codecs._
import scalaz.\/-
import scalaz.-\/
import scodec.bits.BitVector

class CodecEMapTest extends CodecSuite {

  "accept only values no greater than 9" should {

    // accept 8 bit values no greater than 9
    val oneDigit: Codec[Int] = uint8.emap[Int](
      v => if (v > 9) -\/("badv") else \/-(v),
      d => if (d > 9) -\/("badd") else \/-(d))

    "3 is accepted" in {
      oneDigit.encode(3) shouldBe \/-(BitVector(0x03))
    }
    "10 is rejected" in {
      oneDigit.encode(10) shouldBe -\/("badd")
    }
    "too large to fit in 8 bits" in {
      oneDigit.encode(30000000) shouldBe -\/("badd")
    }
    "0x05 is acceptable" in {
      oneDigit.decode(BitVector(0x05)) shouldBe \/-((BitVector.empty, 5))
    }
    "0xff is rejected" in {
      oneDigit.decode(BitVector(0xff)) shouldBe -\/("badv")
    }
    "empty BitVector is rejected" in {
      oneDigit.decode(BitVector.empty) shouldBe uint8.decode(BitVector.empty)
    }
  }
  "acccept any 8 bit value" should {
    
    // empty implementation that accepts all values
    val noop: Codec[Int] = uint8.emap[Int](v => \/-(v),
      d => \/-(d))

    "value that fits in 8 bits" in {
      noop.encode(10) shouldBe \/-(BitVector(0x0a))
    }
    "value that exceeds 8 bits" in {
      noop.encode(30000000) shouldBe uint8.encode(30000000)
    }
  }
}