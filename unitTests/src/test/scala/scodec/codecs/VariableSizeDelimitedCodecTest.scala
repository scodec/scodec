package scodec
package codecs

import scodec.bits._

class VariableSizeDelimitedCodecTest extends CodecSuite {

  test("encode value followed by delimiter") {
    assertEquals(variableSizeDelimited(constant(0), utf8, 8).encode("test"), Attempt.successful(
      BitVector('t', 'e', 's', 't', 0)
    ))
  }

  test("decode value followed by delimiter") {
    val buffer = BitVector('t', 'e', 's', 't', 0)
    assertEquals(variableSizeDelimited(constant(0), ascii, 8).decode(buffer), Attempt.successful(
      DecodeResult("test", BitVector.empty)
    ))
  }

  test("decode value followed by delimiter with remainder") {
    val buffer = BitVector('t', 'e', 's', 't', 0, 1, 2, 3)
    assertEquals(variableSizeDelimited(constant(0), ascii, 8).decode(buffer), Attempt.successful(
      DecodeResult("test", BitVector(1, 2, 3))
    ))
  }
}
