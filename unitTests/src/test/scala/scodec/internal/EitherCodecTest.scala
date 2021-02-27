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

import scodec.bits.*
import scodec.codecs.*

import org.scalacheck.*
import org.scalacheck.Prop.forAll

class EitherCodecTest extends CodecSuite:

  test("roundtrip (1)") {
    val c = either(bool(8), uint8, utf8)
    roundtrip(c, Left(0))
    roundtrip(c, Left(255))
    roundtrip(c, Right("hello, world"))
  }

  property("roundtrip (2)") {
    // locally override Arbitrary[Int] to fit in 8 bytes unsigned
    implicit val arb: Arbitrary[Int] = Arbitrary(Gen.choose(0, 255))
    val c = either(bool(8), uint8, utf8)
    forAll((e: Either[Int, String]) => roundtrip(c, e))
  }

  test("encode") {
    val c = either(bool(8), uint8, ascii)
    assertEquals(c.encode(Left(255)), Attempt.successful(bin"00000000 11111111"))
    assertEquals(c.encode(Right("hi")), Attempt.successful(hex"ff 68 69".toBitVector))
  }
