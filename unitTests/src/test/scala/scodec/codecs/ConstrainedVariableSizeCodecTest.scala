package scodec
package codecs

import scodec.bits._

class ConstrainedVariableSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(
      constrainedVariableSizeBytes(uint8, utf8, 8),
      "" :: "a" :: "ab" :: "abc" :: "abcd" :: "áêçã" :: Nil
    )
    roundtripAll(
      constrainedVariableSizeBytes(uint16, utf8, 8),
      "" :: "a" :: "ab" :: "abc" :: "abcd" :: "áêçã" :: Nil
    )
    roundtripAll(
      constrainedVariableSizeBytes(uint8, utf8, 3, 8),
      "abc" :: "abcd" :: "áêçã" :: Nil
    )
    roundtripAll(
      constrainedVariableSizeBytes(uint16, utf8, 3, 8),
      "abc" :: "abcd" :: "áêçã" :: Nil
    )
  }

  test("forbid encoding under lower boundaries") {
    assertEquals(constrainedVariableSizeBytes(uint8, utf8, 3, 8).encode(""), Attempt.Failure(
      Err("Size out of bounds: 24 <= 0 <= 64 is not true")
    ))
    assertEquals(constrainedVariableSizeBytes(uint16, utf8, 3, 8).encode(""), Attempt.Failure(
      Err("Size out of bounds: 24 <= 0 <= 64 is not true")
    ))
  }

  test("forbid encoding over upper boundaries") {
    assertEquals(constrainedVariableSizeBytes(uint8, utf8, 3).encode("çãéá"), Attempt.Failure(
      Err("Size out of bounds: 0 <= 64 <= 24 is not true")
    ))
    assertEquals(constrainedVariableSizeBytes(uint16, utf8, 3).encode("çãéá"), Attempt.Failure(
      Err("Size out of bounds: 0 <= 64 <= 24 is not true")
    ))
  }

  test("forbid decoding under lower boundaries") {
    assertEquals(constrainedVariableSizeBytes(uint8, utf8, 3, 8).decode(hex"08 30".bits), Attempt
      .Failure(Err("Size out of bounds: 24 <= 8 <= 64 is not true")))
    assertEquals(constrainedVariableSizeBytes(uint16, utf8, 3, 8).decode(hex"00 08 30".bits), Attempt
      .Failure(Err("Size out of bounds: 24 <= 8 <= 64 is not true")))
  }

  test("forbid decoding over upper boundaries") {
    assertEquals(constrainedVariableSizeBytes(uint8, utf8, 2).decode(hex"18 30 30 30".bits), Attempt
      .Failure(Err("Size out of bounds: 0 <= 24 <= 16 is not true")))
    assertEquals(constrainedVariableSizeBytes(uint16, utf8, 2).decode(hex"00 18 30 30 30".bits), Attempt
      .Failure(Err("Size out of bounds: 0 <= 24 <= 16 is not true")))
  }
}
