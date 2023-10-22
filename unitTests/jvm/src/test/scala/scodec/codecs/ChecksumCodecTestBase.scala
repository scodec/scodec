package scodec
package codecs

import scodec.Attempt.Failure
import scodec.Err.InsufficientBits
import scodec.bits.BitVector
import org.scalacheck.Prop.forAll

abstract class ChecksumCodecTestBase extends CodecSuite {

  protected def checkSumString: Codec[String]

  protected def checkSumLong: Codec[Long]

  protected def checkSumLongFramed: Codec[(Long, Long)]

  test("roundtrip undefined size") {
    forAll { (body: String) =>
      val expected = DecodeResult(body, BitVector.empty)
      expected == checkSumString.decode(checkSumString.encode(body).require).require
    }
  }

  test("roundtrip defined size") {
    forAll { (body: Long) =>
      val expected = DecodeResult(body, BitVector.empty)
      expected == checkSumLong.decode(checkSumLong.encode(body).require).require
    }
  }

  test("roundtrip defined size with framing") {
    forAll { (body: (Long, Long)) =>
      val expected = DecodeResult(body, BitVector.empty)
      expected == checkSumLongFramed.decode(checkSumLongFramed.encode(body).require).require
    }
  }

  test("drop bit") {
    forAll { (body: Long) =>
      checkSumLong.decode(checkSumLong.encode(body).require.drop(1)) match {
        case Failure(_: InsufficientBits) => true
        case _                            => false
      }
    }
  }

  test("fail on checksum mismatch") {
    forAll { (body: Long) =>
      val encoded = checkSumLong.encode(body).require
      checkSumLong.decode(encoded.update(0, !encoded(0))) match {
        case Failure(_: ChecksumMismatch) => true
        case _                            => false
      }
    }
  }

  test("extra bits fall into remainder") {
    forAll { (body: Long, extraLong: Long) =>
      val extra = int64.encode(extraLong).require
      val encoded = checkSumLong.encode(body).require ++ extra
      val expected = DecodeResult(body, extra)
      checkSumLong.decode(encoded).require == expected
    }
  }
}
