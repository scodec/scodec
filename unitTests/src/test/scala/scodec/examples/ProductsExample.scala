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
package examples

import scodec.bits._
import codecs._

class ProductsExample extends CodecSuite {

  case class Woozle(count: Int, strength: Int) derives Codec
  case class Wocket(size: Int, inverted: Boolean) derives Codec
  case class Flags(x: Boolean, y: Boolean, z: Boolean) derives Codec

  test("demonstrate case class generation") {
    val codec = Codec[Woozle]
    assertEquals(codec.encode(Woozle(1, 2)).require, hex"0000000100000002".bits)
  }

  test("demonstrate errors in case class codecs are labelled") {
    // Auto generated case class codecs label each component codec
    // with the field name from the case class.
    case class Foo(x: Int)
    object Foo { given Codec[Foo] = uint8.as[Foo] }
    case class Bar(f: Foo) derives Codec
    assertEquals(Codec[Bar].encode(Bar(Foo(500))), Attempt.failure(
      Err("500 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("f")))
  }

  test("demonstrate tuple generation") {
    // You can summon tuple codecs directly
    val codec = Codec[(Int, Int, Boolean)]
    assertBitsEqual(codec.encode(1, 2, true).require, 0x00000001_00000002_ff)
  }

  test("demonstrate use of flatPrepend to encode a dependency on a field") {
    // Let's build a codec for the binary form:
    //  x_included              bool(1)
    //  y_included              bool(1)
    //  z_included              bool(1)
    //                          ignore(5)
    //  if (x_included) {
    //    x                     uint8
    //  }
    //  if (y_included) {
    //    y                     int64
    //  }
    //  if (z_included) {
    //    z                     utf8_32
    //  }

    // We can represent the first byte as a case class of three booleans:
    val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]

    // In order to decode the rest of the binary format, we need access to
    // the decoded flags -- we can do that with flatPrepend
    val codec = flagsCodec.flatPrepend { flgs =>
      conditional(flgs.x, uint8) :: conditional(flgs.y, int64) :: conditional(flgs.z, utf8_32)
    }

    // The type of codec is Flags prepended to whatever tuple the body returned:
    val codecTyped: Codec[(Flags, Option[Int], Option[Long], Option[String])] = codec

    val v = (Flags(true, true, true), Some(1), Some(1L), Some("Hi"))
    assertEquals(codec.encode(v).require, bin"11100000" ++ hex"010000000000000001000000024869".bits)

    // One disadvantage of this approach is that the tuple is inhabited by illegal values.
    // For example, consider encoding the tuple: (Flags(true, true, true), None, None, None).
    // In this example, the tuple claims that x, y, and z are defined but then provides no values for those
    // fields. This is an example of allowing the binary structure leak in to the domain model.
  }

  test("demonstrate use of consume as an alternative to flatPrepend") {
    // To address the disadvantage we ran in to with flatPrepend, we can use consume instead.
    // The consume method is just like flatPrepend in the resulting binary format -- it differs
    // only in the return type of the codec.
    // We need to provide an additional function to consume -- one that is of type `L => A`, or
    // in this case, `(Option[Int], Option[Long], Option[String]) => Flags`.
    // This function will be used to generate a Flags instance when encoding.
    val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
    val codec = flagsCodec.consume { flgs =>
      conditional(flgs.x, uint8) :: conditional(flgs.y, int64) :: conditional(flgs.z, utf8_32)
    } { case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
    assertEquals(codec.encode(Some(1), Some(1L), Some("Hi")).require, bin"11100000" ++ hex"010000000000000001000000024869".bits)
  }

  test("demonstrate use of derive as an alternative to consume") {
    // Sometimes, we have a Codec[(X0, X1, ..., Xn)] and we need to remove one of the
    // components, say Xi, resulting in a Codec[(X0, ..., Xi-1, Xi+1, ..., Xn)],
    // while keeping the binary effect of Xi (that is, the encoding/decoding of Xi should still occur).
    val flagsCodec = (bool :: bool :: bool :: ignore(5)).as[Flags]
    val codec = flagsCodec.consume { flgs =>
      conditional(flgs.x, uint8) :: conditional(flgs.y, int64) :: conditional(flgs.z, utf8_32)
    } { case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
    // In this example, we are deriving X0 for the tuple (X0, X1, X2, X3), but unlike consume,
    // derive works with any tuple component -- not just the head.
    val v = (Some(1), Some(1L), Some("Hi"))
    assertEquals(codec.encode(v).require, bin"11100000" ++ hex"010000000000000001000000024869".bits)
  }
}
