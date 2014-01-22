package scodec

import scalaz.syntax.id._

import org.scalatest._


class ByteVectorTest extends FunSuite with Matchers {
  val deadbeef = ByteVector(0xde, 0xad, 0xbe, 0xef)

  test("toHex") {
    deadbeef.toHex shouldBe "deadbeef"
  }

  test("fromHex") {
    ByteVector.fromHex("0xdeadbeef") should be (deadbeef.right)
    ByteVector.fromHex("0xDEADBEEF") should be (deadbeef.right)
    ByteVector.fromHex("deadbeef") should be (deadbeef.right)
    ByteVector.fromHex("DEADBEEF") should be (deadbeef.right)
    ByteVector.fromHex("de ad be ef") should be (deadbeef.right)
    ByteVector.fromHex("de\tad\nbe\tef") should be (deadbeef.right)
    ByteVector.fromHex("garbage") should be ("Invalid octet 'ga' at position 0".left)
    ByteVector.fromHex("deadbefg") should be ("Invalid octet 'fg' at position 6".left)
  }

  test("toBin") {
    deadbeef.toBin shouldBe "11011110101011011011111011101111"
  }

  test("<<") {
    ByteVector(0x55, 0x55, 0x55) << 1 shouldBe ByteVector(0xaa, 0xaa, 0xaa)
  }

  test(">>") {
    ByteVector(0x55, 0x55, 0x55) >> 1 shouldBe ByteVector(0x2a, 0xaa, 0xaa)
    ByteVector(0xaa, 0xaa, 0xaa) >> 1 shouldBe ByteVector(0xd5, 0x55, 0x55)
  }

  test(">>>") {
    ByteVector(0x55, 0x55, 0x55) >>> 1 shouldBe ByteVector(0x2a, 0xaa, 0xaa)
    ByteVector(0xaa, 0xaa, 0xaa) >>> 1 shouldBe ByteVector(0x55, 0x55, 0x55)
  }
}
