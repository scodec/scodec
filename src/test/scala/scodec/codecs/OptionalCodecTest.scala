package scodec
package codecs

import scalaz.\/-
import scodec.bits.BitVector
import scodec.codecs._

class OptionalCodecTest extends CodecSuite {

  test("produce the target value on true") {
    forAll { (n: Int) =>
      val codec = optional(provide(true), int32)
      val \/-((rest, b)) = codec.decode(BitVector fromInt n)

      rest shouldBe 'empty
      b shouldBe Some(n)
    }
  }

  test("produce none on false") {
    forAll { (n: Int) =>
      val codec = optional(provide(false), int32)
      val \/-((rest, b)) = codec.decode(BitVector fromInt n)

      rest shouldBe BitVector.fromInt(n)
      b shouldBe None
    }
  }
}
