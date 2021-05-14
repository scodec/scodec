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

import scodec.bits.*

class VariableSizeCodecTest extends CodecSuite:

  test("roundtrip") {
    roundtripAll(variableSizeBits(uint8, utf8), Seq("", "test"))
    roundtripAll(variableSizeBits(uint8, utf8, 10), Seq("", "test"))
  }

  test("encode size followed by value") {
    assertEquals(variableSizeBytes(uint8, utf8).encode("test"), Attempt.successful(
      BitVector(4, 't', 'e', 's', 't')
    ))
    assertEquals(variableSizeBits(uint8, utf8).encode("test"), Attempt.successful(
      BitVector(32, 't', 'e', 's', 't')
    ))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    assertEquals(variableSizeBytes(uint2, utf8).encode("too long"), Attempt.failure(
      Err.General(
        "failed to encode size of [too long]: 8 is greater than maximum value 3 for 2-bit unsigned integer",
        List("size")
      )
    ))
  }

  test("support padding of size") {
    assertEquals(variableSizeBits(uint8, uint8, 0).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x08)
    ))
    assertEquals(variableSizeBytes(uint8, uint8, 0).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x01)
    ))
    assertEquals(variableSizeBits(uint8, uint8, 1).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x09)
    ))
    assertEquals(variableSizeBytes(uint8, uint8, 1).encode(0).map(_.take(8)), Attempt.successful(
      BitVector(0x02)
    ))
  }

  test("fail encoding if the wrapped codec returns a bit vector with length not divisible by 8") {
    assertEquals(variableSizeBytes(uint8, bool).encode(true), Attempt.failure(
      Err.General("failed to encode size of [true]: 1 is not evenly divisible by 8", List("size"))
    ))
    assertEquals(variableSizeBytes(uint8, byteAligned(bool)).encode(true), Attempt.successful(
      bin"00000001 10000000"
    ))
  }
