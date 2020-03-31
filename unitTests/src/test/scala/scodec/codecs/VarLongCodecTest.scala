package scodec
package codecs

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class VarLongCodecTest extends CodecSuite {
  def check(low: Long, high: Long, size: Long)(codec: Codec[Long]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  test("vlong - roundtrip")(forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vlong, _)))
  test("vlong - use 1 byte for longs <= 7 bits")(check(0L, 127L, 1)(vlong))
  test("vlong - use 2 bytes for longs <= 14 bits")(check(128L, 16383L, 2)(vlong))
  test("vlong - use 3 bytes for longs <= 21 bits")(check(16384L, 2097151L, 3)(vlong))
  test("vlong - use 4 bytes for longs <= 28 bits")(check(2097152L, 268435455L, 4)(vlong))
  test("vlong - use 5 bytes for longs <= 35 bits")(check(268435456L, 34359738367L, 5)(vlong))
  test("vlong - use 6 bytes for longs <= 42 bits")(check(34359738368L, 4398046511103L, 6)(vlong))
  test("vlong - use 7 bytes for longs <= 49 bits")(check(4398046511104L, 562949953421311L, 7)(vlong))
  test("vlong - use 8 bytes for longs <= 56 bits")(check(562949953421312L, 72057594037927935L, 8)(vlong))
  test("vlong - use 9 bytes for longs <= 63 bits")(check(72057594037927936L, Long.MaxValue, 9)(vlong))

  test("vlongL - roundtrip")(forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vlongL, _)))
  test("vlongL - use 1 byte for longs <= 7 bits")(check(0L, 127L, 1)(vlongL))
  test("vlongL - use 2 bytes for longs <= 14 bits")(check(128L, 16383L, 2)(vlongL))
  test("vlongL - use 3 bytes for longs <= 21 bits")(check(16384L, 2097151L, 3)(vlongL))
  test("vlongL - use 4 bytes for longs <= 28 bits")(check(2097152L, 268435455L, 4)(vlongL))
  test("vlongL - use 5 bytes for longs <= 35 bits")(check(268435456L, 34359738367L, 5)(vlongL))
  test("vlongL - use 6 bytes for longs <= 42 bits")(check(34359738368L, 4398046511103L, 6)(vlongL))
  test("vlongL - use 7 bytes for longs <= 49 bits")(check(4398046511104L, 562949953421311L, 7)(vlongL))
  test("vlongL - use 8 bytes for longs <= 56 bits")(check(562949953421312L, 72057594037927935L, 8)(vlongL))
  test("vlongL - use 9 bytes for longs <= 63 bits")(check(72057594037927936L, Long.MaxValue, 9)(vlongL))
}
