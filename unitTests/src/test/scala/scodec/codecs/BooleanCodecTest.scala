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

class BooleanCodecTest extends CodecSuite:

  test("bool - roundtrip") { roundtripAll(bool, List(true, false)) }
  test("bool - encode") {
    assertEquals(bool.encode(true), Attempt.successful(BitVector.high(1)))
    assertEquals(bool.encode(false), Attempt.successful(BitVector.low(1)))
  }
  test("bool - decode") {
    assertEquals(bool.decode(BitVector.low(1)), Attempt.successful(
      DecodeResult(false, BitVector.empty)
    ))
    assertEquals(bool.decode(BitVector.high(1)), Attempt.successful(
      DecodeResult(true, BitVector.empty)
    ))
    assertEquals(bool.decode(BitVector.low(2)), Attempt.successful(
      DecodeResult(false, BitVector.low(1))
    ))
  }

  test("bool(n) - roundtrip") { roundtripAll(bool(8), List(true, false)) }
  test("bool(n) - encode") {
    assertEquals(bool(8).encode(true), Attempt.successful(BitVector.high(8)))
    assertEquals(bool(8).encode(false), Attempt.successful(BitVector.low(8)))
  }
  test("bool(n) - decode") {
    assertEquals(bool(8).decode(BitVector.low(8)), Attempt.successful(
      DecodeResult(false, BitVector.empty)
    ))
    assertEquals(bool(8).decode(BitVector.high(8)), Attempt.successful(
      DecodeResult(true, BitVector.empty)
    ))
    assertEquals(bool(8).decode(BitVector.low(9)), Attempt.successful(
      DecodeResult(false, BitVector.low(1))
    ))
    assertEquals(bool(8).decode(bin"10000000"), Attempt.successful(DecodeResult(true, BitVector.empty)))
    assertEquals(bool(8).decode(bin"00000001"), Attempt.successful(DecodeResult(true, BitVector.empty)))
  }
  test("bool(n) - return an error when decoding with too few bits") {
    assertEquals(bool(8).decode(BitVector.low(4)), Attempt.failure(Err.insufficientBits(8, 4)))
  }
