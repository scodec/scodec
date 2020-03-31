package scodec
package codecs

import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class OptionalCodecTest extends CodecSuite {

  property("produce the target value on true") {
    forAll { (n: Int) =>
      val codec = optional(provide(true), int32)
      shouldDecodeFullyTo(codec, BitVector.fromInt(n), Some(n))
    }
  }

  property("produce none on false") {
    forAll { (n: Int) =>
      val codec = optional(provide(false), int32)
      val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.fromInt(n))

      assertEquals(rest, BitVector.fromInt(n))
      assertEquals(b, None)
    }
  }
}
