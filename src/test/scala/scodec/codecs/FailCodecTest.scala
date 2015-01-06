package scodec
package codecs

import scodec.bits.BitVector

class FailCodecTest extends CodecSuite {

  // The scalatest fail method shadows this
  def fl[A](e: Err) = scodec.codecs.fail[A](e)

  "the fail codec" should {

    "always fail encoding" in {
      fl(Err("err")).encode(()) shouldBe 'failure
    }

    "always fail decoding" in {
      fl(Err("err")).decode(BitVector.low(1024)) shouldBe 'failure
    }

    "be useful" in {
      val codec: Codec[(Int, String)] = int32 flatZip { x => if (x % 2 == 0) fixedSizeBytes(x / 2, ascii) else fl(Err("must be even")) }
      codec.encode((4, "Hi")) shouldBe 'successful
      codec.encode((1, "Hi")) shouldBe EncodeResult.failure(Err("must be even"))
    }
  }
}
