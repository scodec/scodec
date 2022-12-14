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

class VarLongCodecTest extends CodecSuite:
  def check(low: Long, high: Long, size: Long)(codec: Codec[Long]) =
    forAll(Gen.choose(low, high)) { n =>
      assertEquals(codec.encode(n).map(_.bytes.size), Attempt.successful(size))
    }

  test("vlong - roundtrip")(forAll(Gen.choose(Long.MinValue, Long.MaxValue))(roundtrip(vlong, _)))
  test("vlong - use 1 byte for longs <= 7 bits")(check(0L, 127L, 1)(vlong))
  test("vlong - use 2 bytes for longs <= 14 bits")(check(128L, 16383L, 2)(vlong))
  test("vlong - use 3 bytes for longs <= 21 bits")(check(16384L, 2097151L, 3)(vlong))
  test("vlong - use 4 bytes for longs <= 28 bits")(check(2097152L, 268435455L, 4)(vlong))
  test("vlong - use 5 bytes for longs <= 35 bits")(check(268435456L, 34359738367L, 5)(vlong))
  test("vlong - use 6 bytes for longs <= 42 bits")(check(34359738368L, 4398046511103L, 6)(vlong))
  test("vlong - use 7 bytes for longs <= 49 bits")(
    check(4398046511104L, 562949953421311L, 7)(vlong)
  )
  test("vlong - use 8 bytes for longs <= 56 bits")(
    check(562949953421312L, 72057594037927935L, 8)(vlong)
  )
  test("vlong - use 9 bytes for longs <= 63 bits")(
    check(72057594037927936L, Long.MaxValue, 9)(vlong)
  )
  test("vlong - use 10 bytes for negative longs")(check(Long.MinValue, -1, 10)(vlong))

  test("vlongL - roundtrip")(forAll(Gen.choose(Long.MinValue, Long.MaxValue))(roundtrip(vlongL, _)))
  test("vlongL - use 1 byte for longs <= 7 bits")(check(0L, 127L, 1)(vlongL))
  test("vlongL - use 2 bytes for longs <= 14 bits")(check(128L, 16383L, 2)(vlongL))
  test("vlongL - use 3 bytes for longs <= 21 bits")(check(16384L, 2097151L, 3)(vlongL))
  test("vlongL - use 4 bytes for longs <= 28 bits")(check(2097152L, 268435455L, 4)(vlongL))
  test("vlongL - use 5 bytes for longs <= 35 bits")(check(268435456L, 34359738367L, 5)(vlongL))
  test("vlongL - use 6 bytes for longs <= 42 bits")(check(34359738368L, 4398046511103L, 6)(vlongL))
  test("vlongL - use 7 bytes for longs <= 49 bits")(
    check(4398046511104L, 562949953421311L, 7)(vlongL)
  )
  test("vlongL - use 8 bytes for longs <= 56 bits")(
    check(562949953421312L, 72057594037927935L, 8)(vlongL)
  )
  test("vlongL - use 9 bytes for longs <= 63 bits")(
    check(72057594037927936L, Long.MaxValue, 9)(vlongL)
  )
  test("vlong - use 10 bytes for negative longs")(check(Long.MinValue, -1, 10)(vlongL))
