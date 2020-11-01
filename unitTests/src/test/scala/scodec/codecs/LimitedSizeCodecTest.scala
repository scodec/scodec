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

import scodec.bits._

class LimitedSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(limitedSizeBits(32, utf8), "test")
    roundtrip(limitedSizeBits(8, uint8), 12)
    roundtrip(limitedSizeBits(16, uint8), 12)
  }

  test("not pad") {
    assertEquals(limitedSizeBits(16, uint8).encode(12).require, BitVector(hex"0c"))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    val encoded = utf8.encode("test").require
    assertEquals(limitedSizeBits(32, utf8).decode(encoded ++ BitVector.low(48)), Attempt.successful(
      DecodeResult("test", BitVector.low(48))
    ))
    assertEquals(limitedSizeBits(24, utf8).encode("test"), Attempt.failure(
      Err("[test] requires 32 bits but field is limited to 24 bits")
    ))
  }
}
