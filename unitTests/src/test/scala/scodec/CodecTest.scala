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

import scodec.bits._
import scodec.codecs._
import org.scalacheck.Prop.forAll

class CodecTest extends CodecSuite {
  sealed trait Parent
  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int) extends Parent

  test("flatZip") {
    val codec = uint8.flatZip(n => fixedSizeBits(n.toLong, utf8))
    roundtripAll(codec, Seq((0, ""), (8, "a"), (32, "test")))
  }

  test("complete") {
    val codec = codecs.bits(8)
    assertEquals(codec.decode(hex"00112233".toBitVector), Attempt.successful(DecodeResult(hex"00".bits, hex"112233".bits)))
    assertEquals(codec.complete.decode(hex"00112233".toBitVector), Attempt.failure(Err("24 bits remaining: 0x112233")))
    assertEquals(codec.complete.decode(BitVector.fill(2000)(false)), Attempt.failure(Err("more than 512 bits remaining")))
  }

  test("as - works with tuple codecs of 1 element") {
    roundtripAll(uint8.tuple.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }

  test("as - works with non-tuple codecs") {
    roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }

  test("as - supports destructuring case classes in to tuples") {
    (uint8 :: uint8 :: cstring).as[Foo].as[(Int, Int, String)]
    uint8.tuple.as[Bar].as[Int]
  }

  test("as - supports destructuring singleton case classes in to values") {
    uint8.tuple.as[Bar].as[Int]
  }

  test("as - supports implicitly dropping unit values from a tuple") {
    val c = (uint2 :: uint2 :: ignore(4) :: utf8_32).as[Foo]
    roundtrip(c, Foo(1, 2, "Hi"))
  }

  test("unit") {
    val codec = uint8.unit(0)
    assertEquals(codec.encode(()), Attempt.successful(BitVector(0)))
    assertEquals(codec.decode(BitVector(1)), Attempt.successful(DecodeResult((), BitVector.empty)))
    assertEquals(codec.decode(BitVector.empty), Attempt.failure(Err.InsufficientBits(8, 0, Nil)))
    assertEquals(uint8.unit(255).encode(()), Attempt.successful(BitVector(0xff)))
  }

  test("dropRight") {
    val codec = uint8 <~ uint8.unit(0)
    assertEquals(codec.encode(0xff), Attempt.successful(hex"ff00".bits))
  }

  test("exmap - supports validating input and output") {
    // accept 8 bit values no greater than 9
    val oneDigit: Codec[Int] = uint8.exmap[Int](
      v => if (v > 9) Attempt.failure(Err("badv")) else Attempt.successful(v),
      d => if (d > 9) Attempt.failure(Err("badd")) else Attempt.successful(d)
    )

    assertEquals(oneDigit.encode(3), Attempt.successful(BitVector(0x03)))
    assertEquals(oneDigit.encode(10), Attempt.failure(Err("badd")))
    assertEquals(oneDigit.encode(30000000), Attempt.failure(Err("badd")))
    assertEquals(oneDigit.decode(BitVector(0x05)), Attempt.successful(DecodeResult(5, BitVector.empty)))
    assertEquals(oneDigit.decode(BitVector(0xff)), Attempt.failure(Err("badv")))
    assertEquals(oneDigit.decode(BitVector.empty), uint8.decode(BitVector.empty))
  }

  property("exmap - results in a no-op when mapping successful over both sides") {
    val noop: Codec[Int] = uint8.exmap[Int](Attempt.successful, Attempt.successful)
    forAll((n: Int) => assertEquals(noop.encode(n), uint8.encode(n)))
  }

  def i2l(i: Int): Long = i.toLong
  def l2i(l: Long): Attempt[Int] =
    if (l >= Int.MinValue && l <= Int.MaxValue) Attempt.successful(l.toInt)
    else Attempt.failure(Err("out of range"))

  property("narrow supports converting to a smaller type") {
    val narrowed: Codec[Int] = uint32.narrow(l2i, i2l)
    forAll((n: Int) => assertEquals(narrowed.encode(n), uint32.encode(n.toLong)))
  }

  property("widen supports converting to a larger type") {
    val narrowed = int32.widen(i2l, l2i)
    forAll { (n: Long) =>
      if (n >= Int.MinValue && n <= Int.MaxValue)
        assertEquals(narrowed.encode(n), int32.encode(n.toInt))
      else
        assertEquals(narrowed.encode(n), Attempt.failure(Err("out of range")))
    }
  }

  {
    val char8: Codec[Char] = uint8.contramap[Char](_.toInt).encodeOnly
    test("encodeOnly - encodes successfully") {
      assertEquals(char8.encode('a'), Attempt.successful(BitVector(0x61)))
    }
    test("encodeOnly - fails to decode") {
      assertEquals(char8.decode(hex"61".bits), Attempt.failure(Err("decoding not supported")))
    }
  }

  {
    val char8: Codec[Char] = uint8.map[Char](_.asInstanceOf[Char]).decodeOnly
    test("decodeOnly - decodes successfully") {
      assertEquals(char8.decode(BitVector(0x61)), Attempt.successful(DecodeResult('a', BitVector.empty)))
    }
    test("fails to encode") {
      assertEquals(char8.encode('a'), Attempt.failure(Err("encoding not supported")))
    }
  }

  {
    trait A
    case class B(x: Int) extends A
    case class C(x: Int) extends A
    val codec: Codec[A] = uint8.xmap[B](B.apply, _.x).upcast[A]
    test("upcast - roundtrip values of original type") {
      roundtrip(codec, B(0))
    }
    test("upcast - return an error from encode if passed a different subtype of target type") {
      assertEquals(codec.encode(C(0)).isFailure, true)
    }
    test("upcast - work in presence of nested objects/classes") {
      object X { object Y }
      val c = provide(X).upcast[Any]
      assertEquals(c.encode(X), Attempt.successful(BitVector.empty))
      assert(c.encode(X.Y).isFailure)
    }
  }

  {
    trait A
    case object B extends A
    case object C extends A
    val codec =
      discriminated[A].by(uint8).typecase(1, provide(B)).typecase(2, provide(C)).downcast[B.type]
    test("downcast - roundtrip values of original type") {
      roundtrip(codec, B)
    }
    test("downcast - return an error from decode if decoded value is a supertype of a different type") {
      assertEquals(codec.decode(hex"02".bits).isFailure, true)
    }
    test("downcast - work in presence of nested objects/classes") {
      trait P
      object X extends P { object Y extends P }
      val c = discriminated[P]
        .by(uint8)
        .typecase(0, provide(X))
        .typecase(1, provide(X.Y))
        .downcast[X.type]
      assertEquals(c.decodeValue(hex"00".bits), Attempt.successful(X))
      assert(c.decodeValue(hex"01".bits).isFailure)
    }
  }
}
