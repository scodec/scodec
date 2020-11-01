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

import scala.concurrent.duration._

import munit.{Location, ScalaCheckSuite}
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import scodec.bits.BitVector

abstract class CodecSuite extends ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(100)
      .withWorkers(4)

  protected def roundtrip[A](a: A)(using c: Codec[A], l: Location): Unit =
    roundtrip(c, a)

  protected def roundtrip[A](codec: Codec[A], value: A)(using Location): Unit = {
    val encoded = codec.encode(value)
    assert(encoded.isSuccessful)
    val Attempt.Successful(DecodeResult(decoded, remainder)) = codec.decode(encoded.require)
    assertBitsEqual(remainder, BitVector.empty)
    assert(decoded == value)
    ()
  }

  protected def roundtripAll[A](codec: Codec[A], as: collection.Iterable[A])(using Location): Unit =
    as.foreach(a => roundtrip(codec, a))

  protected def encodeError[A](codec: Codec[A], a: A, err: Err)(using Location) = {
    val encoded = codec.encode(a)
    assert(encoded == Attempt.Failure(err))
  }

  protected def shouldDecodeFullyTo[A](codec: Codec[A], buf: BitVector, expected: A)(using Location) = {
    val Attempt.Successful(DecodeResult(actual, rest)) = codec.decode(buf)
    assertBitsEqual(rest, BitVector.empty)
    assert(actual == expected)
  }

  protected def time[A](f: => A): (A, FiniteDuration) = {
    val start = System.nanoTime
    val result = f
    val elapsed = (System.nanoTime - start).nanos
    (result, elapsed)
  }

  protected def samples[A](gen: Gen[A]): LazyList[Option[A]] =
    LazyList.continually(gen.sample)

  protected def definedSamples[A](gen: Gen[A]): LazyList[A] =
    samples(gen).flatten

  implicit def arbBitVector: Arbitrary[BitVector] =
    Arbitrary(arbitrary[Array[Byte]].map(BitVector.apply))

  protected def assertBitsEqual(actual: BitVector, expected: BitVector)(using Location) =
    assertEquals(actual, expected)
}
