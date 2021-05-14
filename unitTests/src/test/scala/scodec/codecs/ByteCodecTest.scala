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
import scodec.bits.BitVector

class ByteCodecTest extends CodecSuite:
  def check(low: Byte, high: Byte)(f: (Byte) => Unit): Unit =
    forAll(Gen.choose(low, high))(n => f(n))

  property("byte - roundtrip") {
    forAll((n: Byte) => roundtrip(byte, n))
  }

  property("ubyte(n) - roundtrip") {
    forAll(Gen.choose(0, 127))(n => roundtrip(ubyte(7), n.toByte))
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(byte(7).encode(Byte.MaxValue), Attempt.failure(
      Err("127 is greater than maximum value 63 for 7-bit signed byte")
    ))
    assertEquals(byte(7).encode(Byte.MinValue), Attempt.failure(
      Err("-128 is less than minimum value -64 for 7-bit signed byte")
    ))
    assertEquals(ubyte(7).encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 7-bit unsigned byte")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(byte.decode(BitVector.low(4)), Attempt.failure(Err.insufficientBits(8, 4)))
  }
