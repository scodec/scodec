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

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits.*

class PackedDecimalCodecTest extends CodecSuite:
  def check(low: Long, high: Long, size: Long)(codec: Codec[Long]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  property("pbcd - roundtrip") {
    forAll(Gen.choose(0L, Long.MaxValue))(roundtrip(vpbcd, _))
  }

  test("pbcd - decode") {
    assertEquals(
      pbcd(6).decode(hex"010323".bits),
      Attempt.successful(DecodeResult(10323L, BitVector.empty))
    )
  }

  test("pbcd - pad left when encoding (#126)") {
    assertEquals(pbcd(3).encode(0), Attempt.successful(bin"0000 0000 0000"))
    assertEquals(pbcd(3).encode(1), Attempt.successful(bin"0000 0000 0001"))
    assertEquals(pbcd(3).encode(79), Attempt.successful(bin"0000 0111 1001"))
    assertEquals(pbcd(3).encode(999), Attempt.successful(bin"1001 1001 1001"))
  }

  test("lpbcd - encode") {
    val encoded = lpbcd(3).encode(23L)
    assertEquals(encoded, Attempt.successful(hex"023".bits))
    assertEquals(encoded.map(_.bytes), Attempt.successful(hex"0023"))
  }

  test("lpbcd - decode") {
    assertEquals(
      lpbcd(6).decode(hex"010323".bits),
      Attempt.successful(DecodeResult(10323L, BitVector.empty))
    )
    assertEquals(
      lpbcd(5).decode(hex"010323".bits),
      Attempt.successful(DecodeResult(10323L, BitVector.empty))
    )
  }
