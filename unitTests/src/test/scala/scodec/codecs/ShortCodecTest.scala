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

class ShortCodecTest extends CodecSuite:
  def check(low: Short, high: Short)(f: Short => Unit) =
    forAll(Gen.choose(low, high))(n => f(n))

  property("short16 - roundtrip") {
    forAll((n: Short) => roundtrip(short16, n))
  }

  property("short16L - roundtrip") {
    forAll((n: Short) => roundtrip(short16L, n))
  }

  property("ushort(n) - roundtrip") {
    forAll(Gen.choose(0, 32767))(n => roundtrip(ushort(15), n.toShort))
  }

  property("ushortL(n) - roundtrip") {
    forAll(Gen.choose(0, 32767))(n => roundtrip(ushortL(15), n.toShort))
  }

  property("support endianess correctly - (1)") {
    forAll { (n: Short) =>
      val bigEndian = short16.encode(n).require.toByteVector
      val littleEndian = short16L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }

  property("support endianess correctly - (2)") {
    check(0, 8191) { (n: Short) =>
      val bigEndian = ushort(13).encode(n).require
      val littleEndian = ushortL(13).encode(n).require.toByteVector
      val flipped = BitVector(littleEndian.last).take(5) ++ littleEndian.init.reverse.toBitVector
      assertEquals(flipped, bigEndian)
    }
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(short(15).encode(Short.MaxValue), Attempt.failure(
      Err("32767 is greater than maximum value 16383 for 15-bit signed short")
    ))
    assertEquals(short(15).encode(Short.MinValue), Attempt.failure(
      Err("-32768 is less than minimum value -16384 for 15-bit signed short")
    ))
    assertEquals(ushort(15).encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 15-bit unsigned short")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(short16.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(16, 8)))
  }
