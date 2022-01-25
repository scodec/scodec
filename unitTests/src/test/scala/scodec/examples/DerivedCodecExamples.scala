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

import scodec.bits.*
import codecs.*

class DerivedCodecsExample extends CodecSuite:

  test("enums - explicit") {
    enum Color derives Codec { case Red, Green, Blue }
    val c = mappedEnum(uint8, Color.Red -> 10, Color.Green -> 20, Color.Blue -> 30)
    assertBitsEqual(c.encode(Color.Green).require, hex"14".bits)
  }

  test("enums - derived") {
    enum Color derives Codec { case Red, Green, Blue }
    val c = Codec[Color]
    assertBitsEqual(
      (c :: c :: c).encode(Color.Red, Color.Green, Color.Blue).require,
      hex"000102".bits
    )
  }

  sealed trait Sprocket derives Codec
  case class Woozle(count: Int, strength: Int) extends Sprocket derives Codec
  case class Wocket(size: Int, inverted: Boolean) extends Sprocket
  case class Wootle(count: Int, data: BitVector) extends Sprocket

  case class Geiling(name: String, sprockets: Vector[Sprocket]) derives Codec

  sealed trait ColorT
  object ColorT:
    case object Red extends ColorT
    case object Yellow extends ColorT
    case object Green extends ColorT

    given Codec[ColorT] = mappedEnum(uint8, Red -> 0, Yellow -> 1, Green -> 2)

  case class Point(x: Int, y: Int, z: Int)
  object Point:
    given Codec[Point] = (uint8 :: uint8 :: uint8).as[Point]

  test("demonstrate deriving a codec for a case class") {
    // Codecs can be derived automatically for case classes where each component
    // type has a given codec available.
    //
    // In this example, Woozle is a product of two integers
    assertEquals(Codec[Woozle].encode(Woozle(1, 2)).require, hex"0000000100000002".bits)
  }

  test("demonstrate deriving a codec for a sealed class hierarchy") {
    // Codecs can be derived automatically for sealed class hierarchies where:
    //  - each subtype has a given codec (or can have one derived)
    assertEquals(Codec[Sprocket].encode(Wocket(3, true)).require, hex"0100000003ff".bits)
  }

  test("demonstrate nested derivations") {
    // Derived codecs can be based on other derived codecs.
    //
    // Geiling has a Vector[Sprocket] element. The Codec companion object
    // defines a given codec for Vector[A] when there's a given Codec[A] available.
    // There's no manually defined `Codec[Sprocket]` but one is derived.
    val ceil = Geiling("Ceil", Vector(Woozle(1, 2), Wocket(3, true)))
    val encoded = Codec[Geiling].encode(ceil).require
    assertEquals(encoded, hex"00000004 4365696c 00000002 000000000100000002 0100000003ff".bits)
    assertEquals(Codec[Geiling].decode(encoded).require.value, ceil)
  }

  test(
    "demonstrate that derivation support does not interfere with manually authored given codecs in companions"
  ) {
    assertEquals(Codec[ColorT].encode(ColorT.Green).require, hex"02".bits)
    assertEquals(Codec[Point].encode(Point(1, 2, 3)).require.size, 24L)
  }
