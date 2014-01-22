package scodec

import scalaz.syntax.id._

import org.scalatest._


class ByteVectorTest extends FunSuite with Matchers {
  val deadbeef = ByteVector(0xde, 0xad, 0xbe, 0xef)

  test("toHex") {
    deadbeef.toHex shouldBe "deadbeef"
  }

  test("fromHex") {
    ByteVector.fromHex("0xdeadbeef") shouldBe deadbeef.right
    ByteVector.fromHex("0xDEADBEEF") shouldBe deadbeef.right
    ByteVector.fromHex("deadbeef") shouldBe deadbeef.right
    ByteVector.fromHex("DEADBEEF") shouldBe deadbeef.right
    ByteVector.fromHex("de ad be ef") shouldBe deadbeef.right
    ByteVector.fromHex("de\tad\nbe\tef") shouldBe deadbeef.right
    ByteVector.fromHex("garbage") shouldBe "Invalid octet 'ga' at position 0".left
    ByteVector.fromHex("deadbefg") shouldBe "Invalid octet 'fg' at position 6".left
  }

  test("toBin") {
    deadbeef.toBin shouldBe "11011110101011011011111011101111"
  }

  test("fromBin") {
    ByteVector.fromBin(deadbeef.toBin) shouldBe deadbeef.right
    ByteVector.fromBin(deadbeef.toBin.grouped(4).mkString(" ")) shouldBe deadbeef.right
    ByteVector.fromBin("1101a000") shouldBe "Invalid bit 'a' at position 4".left
  }

  test("fromValidBin") {
    ByteVector.fromValidBin(deadbeef.toBin) shouldBe deadbeef
    evaluating { ByteVector.fromValidBin("1101a000") } should produce[IllegalArgumentException]
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

  test("hex string interpolator") {
    hex"deadbeef" shouldBe deadbeef
    val x = "bee"
    hex"dead${x}f" shouldBe deadbeef
    evaluating { hex"deadgg" } should produce[IllegalArgumentException]
  }

}
