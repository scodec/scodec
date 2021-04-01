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

class ByteAlignedCodecTest extends CodecSuite:

  test("roundtrip") {
    roundtrip(byteAligned(int32), Int.MaxValue)
    roundtrip(byteAligned(uint(6)), 2)
  }

  test("pad appropriately") {
    assertEquals(byteAligned(uint(6)).encode(1).require, bin"00000100")
    assertEquals(byteAligned(listOfN(uint(4), uint8))
      .encode(List(1, 2, 3))
      .require, bin"00110000000100000010000000110000")
  }

  test("de-pad appropriately") {
    assertEquals(byteAligned(listOfN(uint(4), uint8))
      .decode(bin"001100000001000000100000001100001111")
      .require, DecodeResult(List(1, 2, 3), bin"1111"))
  }

  test("compute size bounds appropriately") {
    assertEquals(byteAligned(listOfN(uint(4), uint8)).sizeBound, SizeBound.atLeast(8))
  }
