package scodec.codecs

import scodec._
import scodec.bits._

class ConditionalCodecTest extends CodecSuite {

  test("not evaluate if condition is false when encoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(false, retCodec).encode(Some("00"))

    assert(!called)
    assertEquals(result, Attempt.successful(BitVector.empty))
  }

  test("evaluate if condition is true when encoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(true, retCodec).encode(Some("00"))

    assert(called)
    assertEquals(result, Attempt.successful(hex"3030".bits))
  }

  test("not evaluate if condition is false when decoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(false, retCodec).decode(hex"3030".bits)

    assert(!called)
    assertEquals(result, Attempt.successful(DecodeResult(None, hex"3030".bits)))
  }

  test("evaluate if condition is true when decoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(true, retCodec).decode(hex"3030".bits)

    assert(called)
    assertEquals(result, Attempt.successful(DecodeResult(Some("00"), BitVector.empty)))
  }

}
