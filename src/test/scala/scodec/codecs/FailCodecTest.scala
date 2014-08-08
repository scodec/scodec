package scodec
package codecs

import scalaz.{ \/-, -\/ }
import scalaz.std.anyVal.unitInstance

import scodec.bits.BitVector

class FailCodecTest extends CodecSuite {

  // The scalatest fail method shadows this
  def fl[A](msg: String) = scodec.codecs.fail[A](msg)

  test("example usecase") {
    val codec: Codec[(Int, String)] = int32 flatZip { x => if (x % 2 == 0) fixedSizeBytes(x / 2, ascii) else fl("must be even") }
    codec.encode((4, "Hi")) shouldBe 'right
    codec.encode((1, "Hi")) shouldBe -\/("must be even")
  }
}
