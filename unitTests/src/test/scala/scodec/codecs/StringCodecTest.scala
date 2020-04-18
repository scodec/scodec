package scodec
package codecs

import scodec.bits.{BitVector, ByteVector}

class StringCodecTest extends CodecSuite {

  test("ascii - roundtrip") {
    roundtripAll(ascii, Seq("test", "", "with\ttabs"))
  }

  test("ascii32 - roundtrip") {
    roundtripAll(ascii32, Seq("test", "", "with\ttabs"))
  }

  test("ascii32L - roundtrip") {
    roundtripAll(ascii32L, Seq("test", "", "with\ttabs"))
  }

  test("cstring - roundtrip") {
    roundtripAll(cstring, Seq("test", ""))
  }

  test("cstring - decode up to the first null-character") {
    assertEquals(cstring.decode(ascii.encode("hello\u0000").require ++ BitVector.bit(true)),
      Attempt.successful(DecodeResult("hello", BitVector.bit(true)))
    )
  }

  test("cstring - fail decoding with an error when buffer contains bytes unsupported by charset") {
    assertEquals(cstring.decode(ascii.encode("0123456789ABCDEF").require), Attempt.failure(
      Err("Does not contain a 'NUL' termination byte.")
    ))
  }


  test("utf8 - roundtrip") {
    roundtripAll(utf8, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("utf8_32 - roundtrip") {
    roundtripAll(utf8_32, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("utf8_32L - roundtrip") {
    roundtripAll(utf8_32L, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("fail encoding with an error when string to encode contains chars unsupported by charset") {
    assertEquals(ascii.encode("λ"), Attempt.failure(Err("US-ASCII cannot encode character 'λ'")))
    assertEquals(ascii.encode("Includes a λ"), Attempt.failure(
      Err("US-ASCII cannot encode character 'λ'")
    ))
  }

  test("fail decoding with an error when buffer contains bytes unsupported by charset") {
    assertEquals(utf8.decode(BitVector(ByteVector.fromValidHex("0xf4ffffff"))), Attempt.failure(
      Err("UTF-8 cannot decode string from '0xf4ffffff'")
    ))
  }
}
