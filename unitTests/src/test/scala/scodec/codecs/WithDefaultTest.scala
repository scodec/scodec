package scodec
package codecs

import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class WithDefaultTest extends CodecSuite {

  property("decode with fallback codec when opt codec returns none") {
    forAll { (n: Int) =>
      val codec = withDefault(conditional(false, int8), int32)
      shouldDecodeFullyTo(codec, BitVector.fromInt(n), n)
    }
  }

  property("return result of opt codec when it returns some from decode") {
    forAll { (n: Int) =>
      val codec = withDefault(conditional(true, int32), int8)
      shouldDecodeFullyTo(codec, BitVector.fromInt(n), n)
    }
  }

  property("return the default value when the opt codec returns none") {
    forAll { (n: Int) =>
      val codec = withDefaultValue(conditional(false, int8), n)
      val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.fromInt(n))
      assertEquals(rest, (BitVector.fromInt(n)))
      assertEquals(b, n)
    }
  }

  property("return result of opt codec when it returns some from decode") {
    forAll { (n: Int) =>
      val codec = withDefaultValue(conditional(true, int32), n)
      shouldDecodeFullyTo(codec, BitVector.fromInt(n), n)
    }
  }
}
