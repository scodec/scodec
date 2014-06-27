package scodec
package codecs

import scalaz.syntax.id._
import scodec.bits._
import scalaz.\/.fromEither

import org.scalacheck._

class EitherCodecTest extends CodecSuite {

  test("roundtrip") {
    val c = either(bool(8), uint8, utf8)
    val c2 = stdEither(bool(8), uint8, utf8)
    roundtrip(c, 0.left[String])
    roundtrip(c, 255.left[String])
    roundtrip(c, "hello, world".right[Int])
    // locally override Arbitrary[Int] to fit in 8 bytes unsigned
    implicit val arb = Arbitrary(Gen.choose(0,255))
    forAll { (e: Either[Int,String]) => roundtrip(c, fromEither(e)) }
    forAll { (e: Either[Int,String]) => roundtrip(c2, e) }
  }

  test("encode") {
    val c = either(bool(8), uint8, ascii)
    c.encode(255.left) shouldBe bin"00000000 11111111".right
    c.encode("hi".right) shouldBe hex"ff 68 69".toBitVector.right
  }
}


