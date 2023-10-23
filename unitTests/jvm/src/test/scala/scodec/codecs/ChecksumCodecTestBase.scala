/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
