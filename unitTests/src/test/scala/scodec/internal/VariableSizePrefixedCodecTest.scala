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
package internal

import scodec.bits.BitVector
import scodec.codecs.*

class VariableSizePrefixedCodecTest extends CodecSuite:

  test("roundtrip") {
    roundtripAll(variableSizePrefixedBits(uint8, int32, utf8), Seq(2 -> "", 3 -> "test"))
    roundtripAll(variableSizePrefixedBits(uint8, int32, utf8, 10), Seq(2 -> "", 3 -> "test"))
  }

  test("encode size followed by value") {
    assertEquals(variableSizePrefixedBytes(uint8, int32, utf8).encode(2 -> "test"), Attempt.successful(
      BitVector(4, 0, 0, 0, 2, 't', 'e', 's', 't')
    ))
    assertEquals(variableSizePrefixedBits(uint8, int32, utf8).encode(2 -> "test"), Attempt.successful(
      BitVector(32, 0, 0, 0, 2, 't', 'e', 's', 't')
    ))
  }

  test("fail encoding when value is too large to be encoded by size codec") {
    assertEquals(variableSizePrefixedBytes(uint2, int32, utf8).encode(2 -> "too long"), Attempt
      .failure(
        Err(
          "[too long] is too long to be encoded: 8 is greater than maximum value 3 for 2-bit unsigned integer"
        )
      ))
  }

  test("support padding of size") {
    assertEquals(variableSizePrefixedBits(uint8, int32, uint8, 0).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x08)))
    assertEquals(variableSizePrefixedBytes(uint8, int32, uint8, 0).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x01)))

    assertEquals(variableSizePrefixedBits(uint8, int32, uint8, 1).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x09)))
    assertEquals(variableSizePrefixedBytes(uint8, int32, uint8, 1).encode(1 -> 0).map(_.take(8)), Attempt
      .successful(BitVector(0x02)))
  }
