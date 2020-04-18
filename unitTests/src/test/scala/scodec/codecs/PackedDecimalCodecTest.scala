package scodec
package codecs

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits._

class PackedDecimalCodecTest extends CodecSuite {
  def check(low: Long, high: Long, size: Long)(codec: Codec[Long]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  property("pbcd - roundtrip") {
    forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vpbcd, _))
  }

  test("pbcd - decode") {
    assertEquals(pbcd(6).decode(hex"010323".bits),
      Attempt.successful(DecodeResult(10323L, BitVector.empty))
    )
  }

  test("pbcd - pad left when encoding (#126)") {
    assertEquals(pbcd(3).encode(0), Attempt.successful(bin"0000 0000 0000"))
    assertEquals(pbcd(3).encode(1), Attempt.successful(bin"0000 0000 0001"))
    assertEquals(pbcd(3).encode(79), Attempt.successful(bin"0000 0111 1001"))
    assertEquals(pbcd(3).encode(999), Attempt.successful(bin"1001 1001 1001"))
  }

  test("lpbcd - encode") {
    val encoded = lpbcd(3).encode(23L)
    assertEquals(encoded, Attempt.successful(hex"023".bits))
    assertEquals(encoded.map(_.bytes), Attempt.successful(hex"0023"))
  }

  test("lpbcd - decode") {
    assertEquals(lpbcd(6).decode(hex"010323".bits),
      Attempt.successful(DecodeResult(10323L, BitVector.empty))
    )
    assertEquals(lpbcd(5).decode(hex"010323".bits),
      Attempt.successful(DecodeResult(10323L, BitVector.empty))
    )
  }
}
