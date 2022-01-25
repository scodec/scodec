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

class ConstrainedVariableSizeCodecTest extends CodecSuite:

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
    assertEquals(
      constrainedVariableSizeBytes(uint8, utf8, 3, 8).encode(""),
      Attempt.Failure(
        Err("Size out of bounds: 24 <= 0 <= 64 is not true")
      )
    )
    assertEquals(
      constrainedVariableSizeBytes(uint16, utf8, 3, 8).encode(""),
      Attempt.Failure(
        Err("Size out of bounds: 24 <= 0 <= 64 is not true")
      )
    )
  }

  test("forbid encoding over upper boundaries") {
    assertEquals(
      constrainedVariableSizeBytes(uint8, utf8, 3).encode("çãéá"),
      Attempt.Failure(
        Err("Size out of bounds: 0 <= 64 <= 24 is not true")
      )
    )
    assertEquals(
      constrainedVariableSizeBytes(uint16, utf8, 3).encode("çãéá"),
      Attempt.Failure(
        Err("Size out of bounds: 0 <= 64 <= 24 is not true")
      )
    )
  }

  test("forbid decoding under lower boundaries") {
    assertEquals(
      constrainedVariableSizeBytes(uint8, utf8, 3, 8).decode(hex"08 30".bits),
      Attempt
        .Failure(Err("Size out of bounds: 24 <= 8 <= 64 is not true"))
    )
    assertEquals(
      constrainedVariableSizeBytes(uint16, utf8, 3, 8).decode(hex"00 08 30".bits),
      Attempt
        .Failure(Err("Size out of bounds: 24 <= 8 <= 64 is not true"))
    )
  }

  test("forbid decoding over upper boundaries") {
    assertEquals(
      constrainedVariableSizeBytes(uint8, utf8, 2).decode(hex"18 30 30 30".bits),
      Attempt
        .Failure(Err("Size out of bounds: 0 <= 24 <= 16 is not true"))
    )
    assertEquals(
      constrainedVariableSizeBytes(uint16, utf8, 2).decode(hex"00 18 30 30 30".bits),
      Attempt
        .Failure(Err("Size out of bounds: 0 <= 24 <= 16 is not true"))
    )
  }
