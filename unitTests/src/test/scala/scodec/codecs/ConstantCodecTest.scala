package scodec
package codecs

import scodec.bits._

class ConstantCodecTest extends CodecSuite {

  test("constant - fail to decode when codec does not match") {
    assertEquals(constant(1).decode(hex"02".bits), Attempt.failure(
      Err("expected constant BitVector(8 bits, 0x01) but got BitVector(8 bits, 0x02)")
    ))
  }

  test("constantLenient - not fail to decode when codec does not match") {
    assertEquals(constantLenient(1).decode(hex"02".bits), Attempt.successful(
      DecodeResult((), BitVector.empty)
    ))
  }
}
