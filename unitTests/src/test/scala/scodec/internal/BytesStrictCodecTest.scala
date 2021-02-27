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

import scodec.bits.*
import scodec.codecs.*

class BytesStrictCodecTest extends CodecSuite:

  test("roundtrip when input size is correct") { roundtrip(bytesStrict(8), hex"0102030405060708") }

  test("return an error when encoding with less bytes than expected") {
    assertEquals(bytesStrict(8).encode(hex"0102030405"), Attempt.failure(
      Err(
        "[BitVector(40 bits, 0x0102030405)] requires 40 bits but field is fixed size of exactly 64 bits"
      )
    ))
  }

  test("return an error when encoding with more bytes than expected") {
    assertEquals(bytesStrict(8).encode(hex"010203040506070809"), Attempt.failure(
      Err(
        "[BitVector(72 bits, 0x010203040506070809)] requires 72 bits but field is fixed size of exactly 64 bits"
      )
    ))
  }

  test("return an error when decoding with less bytes than expected") {
    assertEquals(bytesStrict(8).decode(BitVector(hex"0102030405")), Attempt.failure(
      Err("expected exactly 64 bits but got 40 bits")
    ))
  }

  test("return an error when decoding with more bytes than expected") {
    assertEquals(bytesStrict(8).decode(BitVector(hex"010203040506070809")), Attempt.failure(
      Err("expected exactly 64 bits but got 72 bits")
    ))
  }
