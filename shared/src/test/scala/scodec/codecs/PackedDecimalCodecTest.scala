package scodec
package codecs

import org.scalacheck.Gen
import scodec.bits._

class PackedDecimalCodecTest extends CodecSuite {
  def check(low: Long, high: Long, size: Int)(codec: Codec[Long]) = {
    forAll(Gen.choose(low, high)) { n =>
      codec.encode(n).map(_.bytes.size) shouldBe Attempt.successful(size)
    }
  }

  "the pbcd codec" should {
    "roundtrip" in forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vpbcd, _))

    "decode correctly" in {
      pbcd(6).decode(hex"010323".bits) should be (Attempt.successful(DecodeResult(10323L, BitVector.empty)))
    }
  }

}
