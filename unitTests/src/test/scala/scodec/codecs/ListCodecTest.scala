package scodec
package codecs

import scodec.bits._

class ListCodecTest extends CodecSuite {

  test("limit decoding to the specified number of records") {
    val codec = listOfN(provide(10), uint8)
    val buffer = BitVector.low(8 * 100)
    assertEquals(codec.decode(buffer), Attempt.successful(
      DecodeResult(List.fill(10)(0), BitVector.low(8 * 90))
    ))
  }

  test("support encoding size before vector contents") {
    val codec = listOfN(int32, uint8)
    assertEquals(codec.encode((1 to 10).toList), Attempt.successful(
      hex"0000000a0102030405060708090a".bits
    ))
  }

  test("support not encoding size before vector contents") {
    val codec = listOfN(provide(10), uint8)
    assertEquals(codec.encode((1 to 10).toList), Attempt.successful(hex"102030405060708090a".bits))
  }

  test("fails decoding if < N elements decoded") {
    val codec = listOfN(provide(10), uint8)
    assertEquals(codec.decode(BitVector.low(8 * 5)), Attempt.failure(
      Err("Insufficient number of elements: decoded 5 but should have decoded 10")
    ))
  }
}
