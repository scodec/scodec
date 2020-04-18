package scodec
package codecs

import scodec.bits._

class BooleanCodecTest extends CodecSuite {

  test("bool - roundtrip") { roundtripAll(bool, List(true, false)) }
  test("bool - encode") {
    assertEquals(bool.encode(true), Attempt.successful(BitVector.high(1)))
    assertEquals(bool.encode(false), Attempt.successful(BitVector.low(1)))
  }
  test("bool - decode") {
    assertEquals(bool.decode(BitVector.low(1)), Attempt.successful(
      DecodeResult(false, BitVector.empty)
    ))
    assertEquals(bool.decode(BitVector.high(1)), Attempt.successful(
      DecodeResult(true, BitVector.empty)
    ))
    assertEquals(bool.decode(BitVector.low(2)), Attempt.successful(
      DecodeResult(false, BitVector.low(1))
    ))
  }

  test("bool(n) - roundtrip") { roundtripAll(bool(8), List(true, false)) }
  test("bool(n) - encode") {
    assertEquals(bool(8).encode(true), Attempt.successful(BitVector.high(8)))
    assertEquals(bool(8).encode(false), Attempt.successful(BitVector.low(8)))
  }
  test("bool(n) - decode") {
    assertEquals(bool(8).decode(BitVector.low(8)), Attempt.successful(
      DecodeResult(false, BitVector.empty)
    ))
    assertEquals(bool(8).decode(BitVector.high(8)), Attempt.successful(
      DecodeResult(true, BitVector.empty)
    ))
    assertEquals(bool(8).decode(BitVector.low(9)), Attempt.successful(
      DecodeResult(false, BitVector.low(1))
    ))
    assertEquals(bool(8).decode(bin"10000000"), Attempt.successful(DecodeResult(true, BitVector.empty)))
    assertEquals(bool(8).decode(bin"00000001"), Attempt.successful(DecodeResult(true, BitVector.empty)))
  }
  test("bool(n) - return an error when decoding with too few bits") {
    assertEquals(bool(8).decode(BitVector.low(4)), Attempt.failure(Err.insufficientBits(8, 4)))
  }
}
