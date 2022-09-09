package scodec
package codecs

import scodec.bits.BitVector

class FailCodecTest extends CodecSuite {

  // The scalatest fail method shadows this
  def fl[A](e: Err) = scodec.codecs.fail[A](e)

  "the fail codec" should {

    "always fail encoding" in {
      fl(Err("err")).encode(()) shouldBe Attempt.failure(Err("err"))
    }

    "always fail decoding" in {
      fl(Err("err")).decode(BitVector.low(1024)) shouldBe Attempt.failure(Err("err"))
    }

    "be useful" in {
      val codec: Codec[(Int, String)] = int32.flatZip { x =>
        if (x % 2 == 0) fixedSizeBytes(x / 2L, ascii) else fl(Err("must be even"))
      }
      codec.encode((4, "Hi")).isSuccessful shouldBe true
      codec.encode((1, "Hi")) shouldBe Attempt.failure(Err("must be even"))
    }
  }
}
