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

import scodec.bits.*

class TupleCodecTest extends CodecSuite:

  case class Foo(x: Int, y: Int, s: String)
  case class Bar(x: Int)
  case class Baz(a: Int, b: Int, c: Int, d: Int)
  case class Flags(x: Boolean, y: Boolean, z: Boolean)

  test("support construction via :: operator") {
    roundtrip((uint8 :: uint8 :: utf8), (1, 2, "test"))
  }

  test("support conversion of tuple codec to a case class codec via as method") {
    roundtrip((uint8 :: uint8 :: utf8).as[Foo], Foo(1, 2, "test"))
  }

  test("support conversion of non-tuple codec to a case class codec via as method") {
    roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }

  test("support converting an tuple of codecs") {
    val a: Codec[(Int, Long, Boolean)] = Codec.fromTuple(uint8, int64, bool)
    roundtrip(a, (1, 2L, true))
  }

  test("provide a flatPrepend method") {
    uint8.flatPrepend(n => bits(n.toLong).tuple)
  }

  test("provide ability to append via :+ operator") {
    roundtrip(((uint8 :: uint8) :+ utf8).as[Foo], Foo(1, 2, "test"))
  }

  test("provide ability to concatenate two tuple codecs") {
    roundtrip(((uint8 :: uint8) ++ (uint8 :: uint8)).as[Baz], Baz(1, 2, 3, 4))
  }

  test("support dropping all Unit values out of a tuple codec") {
    def ign(size: Int) = scodec.codecs.ignore(size.toLong)
    val codec = (uint8 :: ign(8) :: uint8 :: ign(8) :: utf8).dropUnits.as[Foo]
    roundtrip(codec, Foo(1, 2, "test"))
  }

  test("flatConcat") {
    val c = (uint8 :: uint8).flatConcat(_ => uint8 :: uint8)
    roundtrip(c, (1, 2, 3, 4))
  }

  test("flatAppend") {
    val c = (uint8 :: uint8).flatAppend(_ => uint8)
    roundtrip(c, (1, 2, 3))
  }

  test("consume") {
    val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
    val values = flagsCodec.consume { flgs =>
      conditional(flgs.x, uint8) :: conditional(flgs.y, uint8) :: conditional(flgs.z, uint8)
    } { case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
    assertEquals(values.encode(None, None, None), Attempt.successful(bin"00000000"))
    assertEquals(
      values.encode(Some(1), Some(2), Some(3)),
      Attempt.successful(
        bin"11100000 00000001 00000010 00000011"
      )
    )
    assertEquals(
      values.encode(Some(1), None, Some(3)),
      Attempt.successful(
        bin"10100000 00000001 00000011"
      )
    )
    roundtrip(values, (Some(1), Some(2), None))
  }
