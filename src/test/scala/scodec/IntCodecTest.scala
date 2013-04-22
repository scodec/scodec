package scodec

import scalaz.syntax.id._

import org.scalatest._


class IntCodecTest extends FunSuite with Matchers {

  val int32 = new IntCodec(32)
  val int16 = new IntCodec(16)
  val uint16 = new IntCodec(16, signed = false)

  test("roundtrip") {
    Seq(0, 1, -1, Int.MaxValue, Int.MinValue) foreach { n => roundtrip(int32, n) }
    Seq(0, 1, -1, 32767, -32768) foreach { n => roundtrip(int16, n) }
    Seq(0, 1, 65535) foreach { n => roundtrip(uint16, n) }
  }

  test("range checking") {
    int16.encode(65536) shouldBe "65536 is greater than maximum value 32767 for 16-bit signed integer".left
    int16.encode(-32769) shouldBe "-32769 is less than minimum value -32768 for 16-bit signed integer".left
    uint16.encode(-1) shouldBe "-1 is less than minimum value 0 for 16-bit unsigned integer".left
  }

  test("decoding with too few bits") {
    int16.decode(BitVector.low(8)) shouldBe ("cannot acquire 16 bits from a vector that contains 8 bits".left)
  }

  private def roundtrip[A](codec: Codec[A], a: A) {
    val encoded = codec.encode(a)
    encoded should be ('right)
    val decoded = codec.decode(encoded.toOption.get)
    decoded shouldBe (BitVector.empty, a).right
  }
}
