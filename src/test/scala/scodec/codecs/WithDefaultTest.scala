package scodec
package codecs

import scalaz.\/-
import scodec.bits.BitVector
import scodec.codecs._

class WithDefaultTest extends CodecSuite {

  "the withDefault combinator" should {

    "decode with fallback codec when opt codec returns none" in {
      forAll { (n: Int) =>
        val codec = withDefault(conditional(false, int8), int32)
        shouldDecodeFullyTo(codec, BitVector fromInt n, n)
      }
    }

    "return result of opt codec when it returns some from decode" in {
      forAll { (n: Int) =>
        val codec = withDefault(conditional(true, int32), int8)
        shouldDecodeFullyTo(codec, BitVector fromInt n, n)
      }
    }
  }

  "the withDefaultValue combinator" should {

    "return the default value when the opt codec returns none" in {
      forAll { (n: Int) =>
        val codec = withDefaultValue(conditional(false, int8), n)
        val \/-((rest, b)) = codec.decode(BitVector fromInt n)
        rest shouldBe (BitVector fromInt n)
        b shouldBe n
      }
    }

    "return result of opt codec when it returns some from decode" in {
      forAll { (n: Int) =>
        val codec = withDefaultValue(conditional(true, int32), n)
        shouldDecodeFullyTo(codec, BitVector fromInt n, n)
      }
    }
  }
}
