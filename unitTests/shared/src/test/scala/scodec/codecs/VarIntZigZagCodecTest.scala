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

class VarIntZigZagCodecTest extends CodecSuite:

  def check(low: Int, high: Int, size: Long)(codec: Codec[Int]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  test("zint - roundtrip")(forAll(Gen.choose(Int.MinValue, Int.MaxValue))(roundtrip(zint, _)))
  test("zint - use 1 byte for abs ints <= 6 bits")(
    check(-64, -1, 1)(zint) && check(0, 63, 1)(zint)
  )
  test("zint - use 2 bytes for abs ints <= 13 bits")(
    check(-8192, -65, 2)(zint) && check(64, 8191, 2)(zint)
  )
  test("zint - use 3 bytes for abs ints <= 20 bits")(
    check(-1048576, -8193, 3)(zint) && check(8192, 1048575, 3)(zint)
  )
  test("zint - use 4 bytes for abs ints <= 27 bits")(
    check(-134217728, -1048577, 4)(zint) && check(1048576, 134217727, 4)(zint)
  )
  test("zint - use 5 bytes for abs ints <= 31 bits")(
    check(Int.MinValue, -134217729, 5)(zint) && check(134217728, Int.MaxValue, 5)(zint)
  )

  test("zintL - roundtrip")(forAll(Gen.choose(Int.MinValue, Int.MaxValue))(roundtrip(zintL, _)))
  test("zintL - use 1 byte for abs ints <= 6 bits")(
    check(-64, -1, 1)(zintL) && check(0, 63, 1)(zintL)
  )
  test("zintL - use 2 bytes for abs ints <= 13 bits")(
    check(-8192, -65, 2)(zintL) && check(64, 8191, 2)(zintL)
  )
  test("zintL - use 3 bytes for abs ints <= 20 bits")(
    check(-1048576, -8193, 3)(zintL) && check(8192, 1048575, 3)(zintL)
  )
  test("zintL - use 4 bytes for abs ints <= 27 bits")(
    check(-134217728, -1048577, 4)(zintL) && check(1048576, 134217727, 4)(zintL)
  )
  test("zintL - use 5 bytes for abs ints <= 31 bits")(
    check(Int.MinValue, -134217729, 5)(zintL) && check(134217728, Int.MaxValue, 5)(zintL)
  )
