package scodec
package codecs

import scodec.bits._

import org.scalacheck._
import org.scalacheck.Prop.forAll

class EitherCodecTest extends CodecSuite {

  test("roundtrip (1)") {
    val c = either(bool(8), uint8, utf8)
    roundtrip(c, Left(0))
    roundtrip(c, Left(255))
    roundtrip(c, Right("hello, world"))
  }

  property("roundtrip (2)") {
    // locally override Arbitrary[Int] to fit in 8 bytes unsigned
    implicit val arb: Arbitrary[Int] = Arbitrary(Gen.choose(0, 255))
    val c = either(bool(8), uint8, utf8)
    forAll((e: Either[Int, String]) => roundtrip(c, e))
  }

  test("encode") {
    val c = either(bool(8), uint8, ascii)
    assertEquals(c.encode(Left(255)), Attempt.successful(bin"00000000 11111111"))
    assertEquals(c.encode(Right("hi")), Attempt.successful(hex"ff 68 69".toBitVector))
  }
}
