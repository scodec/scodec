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

import scodec.codecs.*

class VarIntCodecTest extends CodecSuite:
  def check(low: Int, high: Int, size: Long)(codec: Codec[Int]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  test("vint - roundtrip")(forAll(Gen.choose(Int.MinValue, Int.MaxValue))(roundtrip(vint, _)))
  test("vint - use 1 byte for ints <= 7 bits")(check(0, 127, 1)(vint))
  test("vint - use 2 bytes for ints <= 14 bits")(check(128, 16383, 2)(vint))
  test("vint - use 3 bytes for ints <= 21 bits")(check(16384, 2097151, 3)(vint))
  test("vint - use 4 bytes for ints <= 28 bits")(check(2097152, 268435455, 4)(vint))
  test("vint - use 5 bytes for ints <= 32 bits")(check(268435456, Int.MaxValue, 5)(vint))
  test("vint - use 5 bytes for negative ints")(check(Int.MinValue, -1, 5)(vint))

  test("vintL - roundtrip")(forAll(Gen.choose(0, Int.MaxValue))(roundtrip(vintL, _)))
  test("vintL - use 1 byte for ints <= 7 bits")(check(0, 127, 1)(vintL))
  test("vintL - use 2 bytes for ints <= 14 bits")(check(128, 16383, 2)(vintL))
  test("vintL - use 3 bytes for ints <= 21 bits")(check(16384, 2097151, 3)(vintL))
  test("vintL - use 4 bytes for ints <= 28 bits")(check(2097152, 268435455, 4)(vintL))
  test("vintL - use 5 bytes for ints <= 32 bits")(check(268435456, Int.MaxValue, 5)(vintL))
  test("vintL - use 5 bytes for negative ints")(check(Int.MinValue, -1, 5)(vintL))
