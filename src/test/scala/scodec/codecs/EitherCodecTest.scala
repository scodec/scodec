package scodec
package codecs

import scalaz.syntax.id._
import scodec.bits._

class EitherCodecTest extends CodecSuite {

  test("roundtrip") {
    val c = either(bool(8), uint8, ascii)
    roundtrip(c, 0.left[String])
    roundtrip(c, 255.left[String])
    roundtrip(c, "hello, world".right[Int])
  }

  test("encode") {
    val c = either(bool(8), uint8, ascii)
    c.encode(255.left) shouldBe bin"00000000 11111111".right
    c.encode("hi".right) shouldBe hex"ff 68 69".toBitVector.right
  }
}


