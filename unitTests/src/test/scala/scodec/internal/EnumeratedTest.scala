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

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import scodec.bits.*
import scodec.codecs.*

class EnumeratedTest extends CodecSuite:

  object SIPrefix extends Enumeration:
    type SIPrefix = Value
    val DEKA = Value
    val HECTO = Value
    val KILO = Value
    val MEGA = Value
    val GIGA = Value

  val codec = enumerated(int32, SIPrefix)
  implicit def generator: Arbitrary[SIPrefix.Value] = Arbitrary(Gen.oneOf(SIPrefix.values.toSeq))

  property("roundtrip") {
    forAll((v: SIPrefix.Value) => roundtrip(codec, v))
  }

  property("roundtrip with combinators") {
    forAll((i: Int, v: SIPrefix.Value) => roundtrip(int32 :: codec, (i, v)))
  }

  test("fail for an invalid id") {
    assert(codec.decode(hex"000000FF".bits).isFailure)
  }
