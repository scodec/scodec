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

class IntCodecTest extends CodecSuite {
  def check(low: Int, high: Int)(f: Int => Unit) =
    forAll(Gen.choose(low, high))(n => f(n))

  property("int32 - roundtrip") {
    forAll((n: Int) => roundtrip(int32, n))
  }

  property("int32L - roundtrip") {
    forAll((n: Int) => roundtrip(int32L, n))
  }

  property("uint24L - roundtrip") {
    check(0, (1 << 24) - 1)((n: Int) => roundtrip(uint24L, n))
  }

  property("int16 - roundtrip") {
    check(-32768, 32767)((n: Int) => roundtrip(int16, n))
  }

  property("uint16 - roundtrip") {
    check(0, 65535)((n: Int) => roundtrip(uint16, n))
  }

  property("uint16L - roundtrip") {
    check(0, 65535)((n: Int) => roundtrip(uint16L, n))
  }

  property("uint8 - roundtrip") {
    check(0, 255)((n: Int) => roundtrip(uint8, n))
  }
  
  property("uint8L - roundtrip") {
    check(0, 255)((n: Int) => roundtrip(uint8L, n))
  }

  property("uint4 - roundtrip") {
    check(0, 1 << 3)((n: Int) => roundtrip(uint4, n))
  }

  property("uint4L - roundtrip") {
    check(0, (1 << 4) - 1)((n: Int) => roundtrip(uint4L, n))
  }

  property("uint(n) - roundtrip") {
    assertEquals(uint(13).encode(1), Attempt.successful(BitVector.low(13).set(12)))
    check(0, 32767)((n: Int) => roundtrip(uint(15), n))
  }

  property("uintL(n) - roundtrip") {
    assertEquals(uintL(13).encode(1), Attempt.successful(BitVector.low(13).set(7)))
    check(0, 32767)((n: Int) => roundtrip(uintL(15), n))
  }

  property("support endianess correctly (1)") {
    forAll { (n: Int) =>
      val bigEndian = int32.encode(n).require.toByteVector
      val littleEndian = int32L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }
  property("support endianess correctly (2)") {
    check(0, 15) { (n: Int) =>
      val bigEndian = uint4.encode(n).require.toByteVector
      val littleEndian = uint4L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }
  property("support endianess correctly (3)") {
    check(0, (1 << 24) - 1) { (n: Int) =>
      val bigEndian = uint24.encode(n).require.toByteVector
      val littleEndian = uint24L.encode(n).require.toByteVector
      assertEquals(littleEndian, bigEndian.reverse)
    }
  }
  property("support endianess correctly (4)") {
    check(0, 8191) { (n: Int) =>
      val bigEndian = uint(13).encode(n).require
      val littleEndian = uintL(13).encode(n).require.toByteVector
      val flipped = BitVector(littleEndian.last).take(5) ++ littleEndian.init.reverse.toBitVector
      assertEquals(flipped, bigEndian)
    }
  }

  test("return an error when value to encode is out of legal range") {
    assertEquals(int16.encode(65536), Attempt.failure(
      Err("65536 is greater than maximum value 32767 for 16-bit signed integer")
    ))
    assertEquals(int16.encode(-32769), Attempt.failure(
      Err("-32769 is less than minimum value -32768 for 16-bit signed integer")
    ))
    assertEquals(uint16.encode(-1), Attempt.failure(
      Err("-1 is less than minimum value 0 for 16-bit unsigned integer")
    ))
  }

  test("return an error when decoding with too few bits") {
    assertEquals(int16.decode(BitVector.low(8)), Attempt.failure(Err.insufficientBits(16, 8)))
  }
}
