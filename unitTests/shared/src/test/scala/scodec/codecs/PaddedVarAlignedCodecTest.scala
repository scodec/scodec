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

class PaddedVarAlignedCodecTest extends CodecSuite:

  test("roundtrip") {
    roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "ab")
    roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "abcde")
    roundtrip(paddedVarAlignedBytes(uint16, ascii, 8), "a")
  }

  test("pad to the correct length") {
    assertBitsEqual(
      paddedVarAlignedBytes(uint8, utf8, 4)
        .encode("a")
        .require,
      hex"0161000000".bits
    )
    assertBitsEqual(
      paddedVarAlignedBytes(uint8, utf8, 4)
        .encode("aaa")
        .require,
      hex"0361616100".bits
    )
    assertBitsEqual(
      paddedVarAlignedBytes(uint8, utf8, 4)
        .encode("aaaa")
        .require,
      hex"461616161".bits
    )
  }

  test("pad on a multiplier") {
    assertBitsEqual(
      paddedVarAlignedBytes(uint8, utf8, 3)
        .encode("aaa")
        .require,
      hex"03616161".bits
    )
    assertBitsEqual(
      paddedVarAlignedBytes(uint8, utf8, 3)
        .encode("aaaa")
        .require,
      hex"04616161610000".bits
    )
  }

  test("ignore padded") {
    assertEquals(
      paddedVarAlignedBytes(uint8, utf8, 4)
        .decode(hex"0161000000".bits)
        .require
        .value,
      "a"
    )
    assertEquals(
      paddedVarAlignedBytes(uint8, utf8, 4)
        .decode(hex"0361616100".bits)
        .require
        .value,
      "aaa"
    )
  }
