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

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits.BitVector
import scodec.codecs.*

class LongCodecTest extends CodecSuite:
  def check(low: Long, high: Long)(f: Long => Unit) =
    forAll(Gen.choose(low, high))(n => f(n))

  property("int64 - roundtrip") {
    forAll((n: Long) => roundtrip(int64, n))
  }

  property("inte64L - roundtrip") {
    forAll((n: Long) => roundtrip(int64L, n))
  }

  property("uint32 - roundtrip") {
    check(0, 1L << (32 - 1))((n: Long) => roundtrip(uint32, n))
  }

  property("uint32L - roundtrip") {
    check(0L, (1L << 32) - 1)((n: Long) => roundtrip(uint32L, n))
  }

  test("ulong(n) - roundtrip") { 
    assertEquals(ulong(13).encode(1), Attempt.successful(BitVector.low(13).set(12)))
  }

  test("ulongL(n) - roundtrip") {
    assertEquals(ulongL(13).encode(1), Attempt.successful(BitVector.low(13).set(7)))
  }

  property("support endianess correctly") {
    forAll { (n: Long) =>
      val bigEndian = int64.encode(n).require.toByteVector
      val littleEndian = int64L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(uint32.encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 32-bit unsigned integer")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(uint32.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(32, 8)))
  }
