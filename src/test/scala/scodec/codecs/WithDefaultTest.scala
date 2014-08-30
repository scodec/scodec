package scodec
package codecs

import scalaz.\/-
import scodec.bits.BitVector
import scodec.codecs._

class WithDefaultTest extends CodecSuite {

  test("withDefault - when opt codec returns none during decoding, the fallback codec is used") {
    forAll { (n: Int) =>
      val codec = withDefault(conditional(false, int8), int32)
      shouldDecodeFullyTo(codec, BitVector fromInt n, n)
    }
  }

  test("withDefault - when opt codec returns some during decoding, the target codec is used") {
    forAll { (n: Int) =>
      val codec = withDefault(conditional(true, int32), int8)
      shouldDecodeFullyTo(codec, BitVector fromInt n, n)
    }
  }

  test("withDefaultValue - when opt codec returns none during decoding, the fallback value is used") {
    forAll { (n: Int) =>
      val codec = withDefaultValue(conditional(false, int8), n)
      val \/-((rest, b)) = codec.decode(BitVector fromInt n)
      rest shouldBe (BitVector fromInt n)
      b shouldBe n
    }
  }

  test("withDefaultValue - when opt codec returns some during decoding, the target codec is used") {
    forAll { (n: Int) =>
      val codec = withDefaultValue(conditional(true, int32), n)
      shouldDecodeFullyTo(codec, BitVector fromInt n, n)
    }
  }
}
