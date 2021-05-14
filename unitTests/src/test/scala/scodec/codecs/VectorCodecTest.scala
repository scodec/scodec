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

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import scodec.bits.*

class VectorCodecTest extends CodecSuite:

  test("performance") {
    val codec = vector(int32).complete
    val trials = 10
    val sizes = List(10, 100, 1000, 10000)
    val results = (1 to trials)
      .map { trial =>
        sizes.map { size =>
          val vec = definedSamples(Gen.listOfN(size, arbitrary[Int]).map(_.toVector)).head
          val (encoded, encodeTime) = time(codec.encode(vec).require)
          //println(s"$trial - encoding $size took $encodeTime")
          val (decoded, decodeTime) = time(codec.decode(encoded).require.value)
          //println(s"$trial - decoding $size took $decodeTime")
          assertEquals(decoded, vec)
          encodeTime + decodeTime
        }
      }
      .drop(1) // drop first iteration to allow for JIT
    val avgs = results.reduceLeft((x, y) => (x.zip(y)).map { case (a, b) => a + b }).map {
      _ / results.size.toLong
    }
    println("Roundtrip averages:")
    sizes.zip(avgs).foreach { case (size, avg) => println(s"  $size - $avg") }
  }

  test("include index of problematic value when an error occurs during decoding") {
    val codec = vector(
      uint8.narrow(
        x => if (x == 0) Attempt.failure(Err("zero disallowed")) else Attempt.successful(x),
        x => x
      )
    ).complete
    val result = codec.decode(hex"010200".bits)
    assertEquals(result, Attempt.failure(Err("zero disallowed").pushContext("2")))
  }

  test("include index of problematic value when an error occurs during encoding") {
    val codec = vector(uint8).complete
    val result = codec.encode(Vector(0, 1, -1))
    assertEquals(result, Attempt.failure(
      Err("-1 is less than minimum value 0 for 8-bit unsigned integer").pushContext("2")
    ))
  }

  test("vectorOfN - limit decoding to the specified number of records") {
    val codec = vectorOfN(provide(10), uint8)
    val buffer = BitVector.low(8 * 100)
    assertEquals(codec.decode(buffer), Attempt.successful(
      DecodeResult(Vector.fill(10)(0), BitVector.low(8 * 90))
    ))
  }

  test("support encoding size before vector contents") {
    val codec = vectorOfN(int32, uint8)
    assertEquals(codec.encode((1 to 10).toVector), Attempt.successful(
      hex"0000000a0102030405060708090a".bits
    ))
  }

  test("support not encoding size before vector contents") {
    val codec = vectorOfN(provide(10), uint8)
    assertEquals(codec.encode((1 to 10).toVector), Attempt.successful(hex"102030405060708090a".bits))
  }

  test("fails decoding if < N elements decoded") {
    val codec = vectorOfN(provide(10), uint8)
    assertEquals(codec.decode(BitVector.low(8 * 5)), Attempt.failure(
      Err.insufficientBits(80, 40)
    ))
  }
