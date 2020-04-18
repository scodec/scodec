package scodec
package codecs

import scodec.bits.BitVector

class FailCodecTest extends CodecSuite {

  // The scalatest fail method shadows this
  def fl[A](e: Err) = scodec.codecs.fail[A](e)

  test("always fail encoding") {
    assertEquals(fl(Err("err")).encode(()), Attempt.failure(Err("err")))
  }

  test("always fail decoding") {
    assertEquals(fl(Err("err")).decode(BitVector.low(1024)), Attempt.failure(Err("err")))
  }

  test("example usefulness") {
    val codec: Codec[(Int, String)] = int32.flatZip { x =>
      if (x % 2 == 0) fixedSizeBytes(x / 2L, ascii) else fl(Err("must be even"))
    }
    assertEquals(codec.encode((4, "Hi")).isSuccessful, true)
    assertEquals(codec.encode((1, "Hi")), Attempt.failure(Err("must be even")))
  }
}
