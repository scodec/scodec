package scodec
package codecs

import java.nio.CharBuffer
import java.nio.charset.{Charset, StandardCharsets}

import scodec.bits._

class TerminatedByCodecTest extends CodecSuite {

  implicit def charset: Charset = StandardCharsets.US_ASCII
  implicit def char2ByteVector(c: Char)(implicit C: Charset): ByteVector = ByteVector(C.encode(CharBuffer.wrap(Array(c))))
  implicit def string2ByteVector(s: String)(implicit C: Charset): ByteVector = ByteVector(C.encode(s))

  "terminatedBy codec" should {
    "roundtrip" in {
      roundtripAll(ascii \ hex"23", Seq("", "frame", "f r a m e"))
    }
  }

  "terminatedBy codec" should {
    "roundtrip when payload contains terminal character" in {
      roundtripAll(constant(' ') ~> (constant('[') ~> ascii \ ']') \ ' ', Seq(" ", "frame", "f r a m e"))
    }
  }

  "terminatedBy codec" should {
    "roundtrip when payload contains terminal string" in {
      implicit def charset: Charset = StandardCharsets.US_ASCII
      roundtripAll(constant(' ') ~> (constant('[') ~> ascii \ "]") \ " ", Seq(" ", "frame", "f r a m e"))
    }
  }
}
