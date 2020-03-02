package scodec
package codecs

import scodec.bits._

import org.scalacheck._

class EitherCodecTest extends CodecSuite {

  "the either codec" should {
    "roundtrip" in {
      val c = either(bool(8), uint8, utf8)
      roundtrip(c, Left(0))
      roundtrip(c, Left(255))
      roundtrip(c, Right("hello, world"))
      // locally override Arbitrary[Int] to fit in 8 bytes unsigned
      implicit val arb = Arbitrary(Gen.choose(0, 255))
      forAll((e: Either[Int, String]) => roundtrip(c, e))
    }

    "encode correctly" in {
      val c = either(bool(8), uint8, ascii)
      c.encode(Left(255)) shouldBe Attempt.successful(bin"00000000 11111111")
      c.encode(Right("hi")) shouldBe Attempt.successful(hex"ff 68 69".toBitVector)
    }
  }
}
