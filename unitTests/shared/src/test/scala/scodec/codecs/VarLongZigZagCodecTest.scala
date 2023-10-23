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

class VarLongZigZagCodecTest extends CodecSuite:

  def check(low: Long, high: Long, size: Long)(codec: Codec[Long]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  test("zlong - roundtrip")(forAll(Gen.choose(Long.MinValue, Long.MaxValue))(roundtrip(zlong, _)))
  test("zlong - use 1 byte for abs longs <= 6 bits")(
    check(-64L, -1L, 1)(zlong) && check(0L, 63L, 1)(zlong)
  )
  test("zlong - use 2 bytes for abs longs <= 13 bits")(
    check(-8192L, -65L, 2)(zlong) && check(64L, 8191L, 2)(zlong)
  )
  test("zlong - use 3 bytes for abs longs <= 20 bits")(
    check(-1048576L, -8193L, 3)(zlong) && check(8192L, 1048575L, 3)(zlong)
  )
  test("zlong - use 4 bytes for abs longs <= 27 bits")(
    check(-134217728L, -1048577L, 4)(zlong) && check(1048576L, 134217727L, 4)(zlong)
  )
  test("zlong - use 5 bytes for abs longs <= 34 bits")(
    check(-17179869184L, -134217729L, 5)(zlong) && check(134217728L, 17179869183L, 5)(zlong)
  )
  test("zlong - use 6 bytes for abs longs <= 41 bits")(
    check(-2199023255552L, -17179869185L, 6)(zlong) && check(17179869184L, 2199023255551L, 6)(zlong)
  )
  test("zlong - use 7 bytes for abs longs <= 48 bits")(
    check(-281474976710656L, -2199023255553L, 7)(zlong) &&
      check(2199023255552L, 281474976710655L, 7)(zlong)
  )
  test("zlong - use 8 bytes for abs longs <= 55 bits")(
    check(-36028797018963968L, -281474976710657L, 8)(zlong) &&
      check(281474976710656L, 36028797018963967L, 8)(zlong)
  )
  test("zlong - use 9 bytes for abs longs <= 62 bits")(
    check(-4611686018427387904L, -36028797018963969L, 9)(zlong) &&
      check(36028797018963968L, 4611686018427387903L, 9)(zlong)
  )
  test("zlong - use 10 bytes for abs longs <= 63 bits")(
    check(Long.MinValue, -4611686018427387905L, 10)(zlong) &&
      check(4611686018427387904L, Long.MaxValue, 10)(zlong)
  )

  test("zlongL - roundtrip")(forAll(Gen.choose(Long.MinValue, Long.MaxValue))(roundtrip(zlongL, _)))
  test("zlongL - use 1 byte for abs longs <= 6 bits")(
    check(-64L, -1L, 1)(zlongL) && check(0L, 63L, 1)(zlongL)
  )
  test("zlongL - use 2 bytes for abs longs <= 13 bits")(
    check(-8192L, -65L, 2)(zlongL) && check(64L, 8191L, 2)(zlongL)
  )
  test("zlongL - use 3 bytes for abs longs <= 20 bits")(
    check(-1048576L, -8193L, 3)(zlongL) && check(8192L, 1048575L, 3)(zlongL)
  )
  test("zlongL - use 4 bytes for abs longs <= 27 bits")(
    check(-134217728L, -1048577L, 4)(zlongL) && check(1048576L, 134217727L, 4)(zlongL)
  )
  test("zlongL - use 5 bytes for abs longs <= 34 bits")(
    check(-17179869184L, -134217729L, 5)(zlongL) && check(134217728L, 17179869183L, 5)(zlongL)
  )
  test("zlongL - use 6 bytes for abs longs <= 41 bits")(
    check(-2199023255552L, -17179869185L, 6)(zlongL) &&
      check(17179869184L, 2199023255551L, 6)(zlongL)
  )
  test("zlongL - use 7 bytes for abs longs <= 48 bits")(
    check(-281474976710656L, -2199023255553L, 7)(zlongL) &&
      check(2199023255552L, 281474976710655L, 7)(zlongL)
  )
  test("zlongL - use 8 bytes for abs longs <= 55 bits")(
    check(-36028797018963968L, -281474976710657L, 8)(zlongL) &&
      check(281474976710656L, 36028797018963967L, 8)(zlongL)
  )
  test("zlongL - use 9 bytes for abs longs <= 62 bits")(
    check(-4611686018427387904L, -36028797018963969L, 9)(zlongL) &&
      check(36028797018963968L, 4611686018427387903L, 9)(zlongL)
  )
  test("zlongL - use 10 bytes for abs longs <= 63 bits")(
    check(Long.MinValue, -4611686018427387905L, 10)(zlongL) &&
      check(4611686018427387904L, Long.MaxValue, 10)(zlongL)
  )
