package scodec
package codecs

import org.scalacheck.Gen

class VarLongCodecTest extends CodecSuite {
  def check(low: Long, high: Long, size: Int)(codec: Codec[Long]) =
    forAll(Gen.choose(low, high)) { n =>
      codec.encode(n).map(_.bytes.size) shouldBe Attempt.successful(size)
    }

  "the vlong codec" should {
    "roundtrip" in forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vlong, _))

    "use 1 byte for longs <= 7 bits" in check(0L, 127L, 1)(vlong)
    "use 2 bytes for longs <= 14 bits" in check(128L, 16383L, 2)(vlong)
    "use 3 bytes for longs <= 21 bits" in check(16384L, 2097151L, 3)(vlong)
    "use 4 bytes for longs <= 28 bits" in check(2097152L, 268435455L, 4)(vlong)
    "use 5 bytes for longs <= 35 bits" in check(268435456L, 34359738367L, 5)(vlong)
    "use 6 bytes for longs <= 42 bits" in check(34359738368L, 4398046511103L, 6)(vlong)
    "use 7 bytes for longs <= 49 bits" in check(4398046511104L, 562949953421311L, 7)(vlong)
    "use 8 bytes for longs <= 56 bits" in check(562949953421312L, 72057594037927935L, 8)(vlong)
    "use 9 bytes for longs <= 63 bits" in check(72057594037927936L, Long.MaxValue, 9)(vlong)
  }

  "the vlongL codec" should {
    "roundtrip" in forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vlongL, _))

    "use 1 byte for longs <= 7 bits" in check(0L, 127L, 1)(vlongL)
    "use 2 bytes for longs <= 14 bits" in check(128L, 16383L, 2)(vlongL)
    "use 3 bytes for longs <= 21 bits" in check(16384L, 2097151L, 3)(vlongL)
    "use 4 bytes for longs <= 28 bits" in check(2097152L, 268435455L, 4)(vlongL)
    "use 5 bytes for longs <= 35 bits" in check(268435456L, 34359738367L, 5)(vlongL)
    "use 6 bytes for longs <= 42 bits" in check(34359738368L, 4398046511103L, 6)(vlongL)
    "use 7 bytes for longs <= 49 bits" in check(4398046511104L, 562949953421311L, 7)(vlongL)
    "use 8 bytes for longs <= 56 bits" in check(562949953421312L, 72057594037927935L, 8)(vlongL)
    "use 9 bytes for longs <= 63 bits" in check(72057594037927936L, Long.MaxValue, 9)(vlongL)
  }
}
