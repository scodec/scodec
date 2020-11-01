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

class PaddedVarAlignedCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "ab")
    roundtrip(paddedVarAlignedBytes(uint8, utf8, 4), "abcde")
    roundtrip(paddedVarAlignedBytes(uint16, ascii, 8), "a")
  }

  test("pad to the correct length") {
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 4)
      .encode("a")
      .require, 0x0161000000)
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 4)
      .encode("aaa")
      .require, 0x0361616100)
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 4)
      .encode("aaaa")
      .require, 0x461616161)
  }

  test("pad on a multiplier") {
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 3)
      .encode("aaa")
      .require, 0x03616161)
    assertBitsEqual(paddedVarAlignedBytes(uint8, utf8, 3)
      .encode("aaaa")
      .require, 0x04616161610000)
  }

  test("ignore padded") {
    assertEquals(paddedVarAlignedBytes(uint8, utf8, 4)
      .decode(0x0161000000)
      .require
      .value, "a")
    assertEquals(paddedVarAlignedBytes(uint8, utf8, 4)
      .decode(0x0361616100)
      .require
      .value, "aaa")
  }
}
