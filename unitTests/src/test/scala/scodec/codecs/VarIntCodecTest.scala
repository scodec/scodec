package scodec
package codecs

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class VarIntCodecTest extends CodecSuite {
  def check(low: Int, high: Int, size: Long)(codec: Codec[Int]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  test("vint - roundtrip")(forAll(Gen.choose(Int.MinValue, Int.MaxValue))(roundtrip(vint, _)))
  test("vint - use 1 byte for ints <= 7 bits")(check(0, 127, 1)(vint))
  test("vint - use 2 bytes for ints <= 14 bits")(check(128, 16383, 2)(vint))
  test("vint - use 3 bytes for ints <= 21 bits")(check(16384, 2097151, 3)(vint))
  test("vint - use 4 bytes for ints <= 28 bits")(check(2097152, 268435455, 4)(vint))
  test("vint - use 5 bytes for ints <= 32 bits")(check(268435456, Int.MaxValue, 5)(vint))
  test("vint - use 5 bytes for negative ints")(check(Int.MinValue, -1, 5)(vint))

  test("vintL - roundtrip")(forAll(Gen.choose(0, Int.MaxValue))(roundtrip(vintL, _)))
  test("vintL - use 1 byte for ints <= 7 bits")(check(0, 127, 1)(vintL))
  test("vintL - use 2 bytes for ints <= 14 bits")(check(128, 16383, 2)(vintL))
  test("vintL - use 3 bytes for ints <= 21 bits")(check(16384, 2097151, 3)(vintL))
  test("vintL - use 4 bytes for ints <= 28 bits")(check(2097152, 268435455, 4)(vintL))
  test("vintL - use 5 bytes for ints <= 32 bits")(check(268435456, Int.MaxValue, 5)(vintL))
  test("vintL - use 5 bytes for negative ints")(check(Int.MinValue, -1, 5)(vintL))
}
