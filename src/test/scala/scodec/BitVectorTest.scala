package scodec

import org.scalatest._

class BitVectorTest extends FunSuite with Matchers {

  test("construction via high") {
    BitVector.high(1).toByteVector shouldBe ByteVector(0x80)
    BitVector.high(2).toByteVector shouldBe ByteVector(0xc0)
    BitVector.high(3).toByteVector shouldBe ByteVector(0xe0)
    BitVector.high(4).toByteVector shouldBe ByteVector(0xf0)
    BitVector.high(5).toByteVector shouldBe ByteVector(0xf8)
    BitVector.high(6).toByteVector shouldBe ByteVector(0xfc)
    BitVector.high(7).toByteVector shouldBe ByteVector(0xfe)
    BitVector.high(8).toByteVector shouldBe ByteVector(0xff)
    BitVector.high(9).toByteVector shouldBe ByteVector(0xff, 0x80)
    BitVector.high(10).toByteVector shouldBe ByteVector(0xff, 0xc0)
  }

  test("empty toByteVector") {
    BitVector.empty.toByteVector shouldBe ByteVector.empty
  }

  test("apply") {
    val vec = BitVector(ByteVector(0xf0, 0x0f))
    vec(0) should be (true)
    vec(1) should be (true)
    vec(2) should be (true)
    vec(3) should be (true)
    vec(4) should be (false)
    vec(5) should be (false)
    vec(6) should be (false)
    vec(7) should be (false)
    vec(8) should be (false)
    vec(9) should be (false)
    vec(10) should be (false)
    vec(11) should be (false)
    vec(12) should be (true)
    vec(13) should be (true)
    vec(14) should be (true)
    vec(15) should be (true)
  }

  test("updated") {
    val vec = BitVector.low(16)
    vec.set(6).get(6) should be (true)
    vec.set(10).get(10) should be (true)
    vec.set(10).clear(10).get(10) should be (false)
  }

  test("drop") {
    BitVector.high(8).drop(4).toByteVector shouldBe ByteVector(0xf0)
    BitVector.high(8).drop(3).toByteVector shouldBe ByteVector(0xf8)
    BitVector.high(10).drop(3).toByteVector shouldBe ByteVector(0xfe)
    BitVector.high(12).drop(3).toByteVector shouldBe ByteVector(0xff, 0x80)
    BitVector.empty.drop(4) shouldBe BitVector.empty
    BitVector.high(4).drop(8) shouldBe BitVector.empty
  }

  test("take") {
    BitVector.high(8).take(4).toByteVector shouldBe ByteVector(0xf0)
    BitVector.high(8).take(5).toByteVector shouldBe ByteVector(0xf8)
    BitVector.high(10).take(7).toByteVector shouldBe ByteVector(0xfe)
    BitVector.high(12).take(9).toByteVector shouldBe ByteVector(0xff, 0x80)
    BitVector.high(4).take(100).toByteVector shouldBe ByteVector(0xf0)
  }

  test("dropRight") {
    BitVector.high(12).clear(0).dropRight(4).toByteVector shouldBe ByteVector(0x7f)
  }

  test("takeRight") {
    BitVector.high(12).clear(0).takeRight(4).toByteVector shouldBe ByteVector(0xf0)
  }

  test("++") {
    (BitVector.low(7) ++ BitVector.high(1)).toByteVector shouldBe ByteVector(1: Byte)
    (BitVector.high(8) ++ BitVector.high(8)).toByteVector shouldBe ByteVector(-1: Byte, -1: Byte)
    (BitVector.high(4) ++ BitVector.low(4)).toByteVector shouldBe ByteVector(0xf0)
    (BitVector.high(4) ++ BitVector.high(4)).toByteVector shouldBe ByteVector(-1: Byte)
    (BitVector.high(4) ++ BitVector.high(5)).toByteVector shouldBe ByteVector(-1: Byte, 0x80)
    (BitVector.low(2) ++ BitVector.high(4)).toByteVector shouldBe ByteVector(0x3c)
    (BitVector.low(2) ++ BitVector.high(4) ++ BitVector.low(2)).toByteVector shouldBe ByteVector(0x3c)
  }

  test("<<") {
    (BitVector.high(8) << 0).toByteVector shouldBe ByteVector(0xff)
    (BitVector.high(8) << 4).toByteVector shouldBe ByteVector(0xf0)
    (BitVector.high(10) << 1).toByteVector shouldBe ByteVector(0xff, 0x80)
    (BitVector.high(10) << 3).toByteVector shouldBe ByteVector(0xfe, 0x00)
    (BitVector.high(32) << 16).toByteVector shouldBe ByteVector(0xff, 0xff, 0, 0)
    (BitVector.high(32) << 15).toByteVector shouldBe ByteVector(0xff, 0xff, 0x80, 0)
  }

  test(">>") {
    (BitVector.high(8) >> 8) shouldBe BitVector.high(8)
  }

  test(">>>") {
    (BitVector.high(8) >>> 7).toByteVector shouldBe ByteVector(0x01)
  }

  test("padTo") {
    BitVector.high(2).padTo(8).toByteVector shouldBe ByteVector(0xc0)
    BitVector.high(16).padTo(32).toByteVector shouldBe ByteVector(0xff, 0xff, 0, 0)
  }

  test("~") {
    ~BitVector.high(12) shouldBe BitVector.low(12)
    ~BitVector.low(12) shouldBe BitVector.high(12)
    ~BitVector(10, 10) shouldBe BitVector(245, 245)
    ~BitVector(245, 245) shouldBe BitVector(10, 10)
  }

  test("&") {
    BitVector.high(16) & BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.low(16) & BitVector.high(16) shouldBe BitVector.low(16)
    BitVector.high(16) & BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.low(16) & BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.high(16) & BitVector.high(9) shouldBe BitVector.high(9)
  }

  test("|") {
    BitVector.high(16) | BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.low(16) | BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.high(16) | BitVector.low(16) shouldBe BitVector.high(16)
    BitVector.low(16) | BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.high(16) | BitVector.low(9) shouldBe BitVector.high(9)
  }

  test("^") {
    BitVector.high(16) ^ BitVector.high(16) shouldBe BitVector.low(16)
    BitVector.low(16) ^ BitVector.high(16) shouldBe BitVector.high(16)
    BitVector.high(16) ^ BitVector.low(16) shouldBe BitVector.high(16)
    BitVector.low(16) ^ BitVector.low(16) shouldBe BitVector.low(16)
    BitVector.high(16) ^ BitVector.low(9) shouldBe BitVector.high(9)
    BitVector(10, 245) ^ BitVector(245, 10) shouldBe BitVector.high(16)
  }

  test("toIterable") {
    BitVector.high(8).toIterable
  }

  test("reverse") {
    BitVector(0x03).reverse shouldBe BitVector(0xc0)
    BitVector(0x03, 0x80).reverse shouldBe BitVector(0x01, 0xc0)
    BitVector(0x01, 0xc0).reverse shouldBe BitVector(0x03, 0x80)
  }
}
