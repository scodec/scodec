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

import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class BigIntCodecTest extends CodecSuite:

  property("uint64 - roundtrip") {
    forAll { (n0: BigInt) =>
      val n = n0 % BigInt(1 << 64)
      roundtrip(uint64, n)
    }
  }

  property("uint64L - roundtrip") {
    forAll { (n0: BigInt) =>
      val n = n0 % BigInt(1 << 64)
      roundtrip(uint64L, n)
    }
  }

  property("bigint(256) - roundtrip") {
    forAll((n: BigInt) => roundtrip(bigint(256), n))
  }

  property("bigintL(256) - roundtrip") {
    forAll((n: Long) => roundtrip(bigintL(256), n))
  }

  property("bigint - roundtrip") {
    forAll((n: BigInt) => roundtrip(bigint, n))
  }

  property("bigintL - roundtrip") {
    forAll((n: Long) => roundtrip(bigintL, n))
  }

  property("support endianess correctly - sized") {
    forAll { (n: BigInt) =>
      val bigEndian = bigint(256).encode(n).require
      val littleEndian = bigintL(256).encode(n).require
      assertEquals(littleEndian, bigEndian.reverseByteOrder)
    }
  }

  property("support endianess correctly - unsized") {
    forAll { (n: BigInt) =>
      val bigEndian = bigint.encode(n).require
      val littleEndian = bigintL.encode(n).require
      assertEquals(littleEndian, bigEndian.reverseByteOrder)
    }
  }

  test("return an error when value to encode is out of legal range - sized") {
    assertEquals(
      ubigint(256).encode(-1),
      Attempt.failure(
        Err("-1 is less than minimum value 0 for 256-bit unsigned integer")
      )
    )
  }

  test("return an error when value to encode is out of legal range - unsized") {
    assertEquals(
      ubigint.encode(-1),
      Attempt.failure(
        Err("-1 is less than minimum value 0 for unsized unsigned integer")
      )
    )
  }

  test("return an error when decoding with too few bits") {
    assertEquals(
      bigint(256).decode(BitVector.low(8)),
      Attempt.failure(Err.insufficientBits(256, 8))
    )
  }
