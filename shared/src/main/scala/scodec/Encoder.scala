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

import scala.annotation.unchecked.uncheckedVariance

import scodec.bits.BitVector

/**
  * Supports encoding a value of type `A` to a `BitVector`.
  *
  * @groupname primary Primary Members
  * @groupprio primary 0
  *
  * @groupname combinators Basic Combinators
  * @groupprio combinators 10
  */
trait Encoder[-A] { self =>

  /**
    * Attempts to encode the specified value in to a bit vector.
    *
    * @param value value to encode
    * @return error or binary encoding of the value
    * @group primary
    */
  def encode(value: A): Attempt[BitVector]

  /**
    * Provides a bound on the size of successfully encoded values.
    *
    * @group primary
    */
  def sizeBound: SizeBound

  /**
    * Converts this encoder to an `Encoder[B]` using the supplied `B => A`.
    * @group combinators
    */
  def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def sizeBound = self.sizeBound
    def encode(b: B) = self.encode(f(b))
  }

  /**
    * Converts this encoder to an `Encoder[B]` using the supplied partial
    * function from `B` to `A`. The encoding will fail for any `B` that
    * `f` maps to `None`.
    * @group combinators
    */
  def pcontramap[B](f: B => Option[A]): Encoder[B] = new Encoder[B] {
    def sizeBound = self.sizeBound
    def encode(b: B): Attempt[BitVector] =
      f(b).map(self.encode).getOrElse(Attempt.failure(Err(s"widening failed: $b")))
  }

  /**
    * Converts this encoder to an `Encoder[B]` using the supplied `B => Attempt[A]`.
    * @group combinators
    */
  def econtramap[B](f: B => Attempt[A]): Encoder[B] = new Encoder[B] {
    def sizeBound = self.sizeBound
    def encode(b: B) = f(b).flatMap(self.encode)
  }

  /**
    * Converts this encoder to a new encoder that compacts the generated bit vector before returning it
    * @group combinators
    */
  def compact: Encoder[A] = new Encoder[A] {
    def sizeBound = self.sizeBound
    def encode(a: A) = self.encode(a).map(_.compact)
  }

  /**
    * Gets this as an `Encoder`.
    * @group combinators
    */
  def asEncoder: Encoder[A] = this

  /**
    * Converts this to a codec that fails decoding with an error.
    * @group combinators
    */
  def encodeOnly: Codec[A @uncheckedVariance] = new Codec[A] {
    def sizeBound = self.sizeBound
    def encode(a: A) = self.encode(a)
    def decode(bits: BitVector) = Attempt.failure(Err("decoding not supported"))
  }

  /**
    * Encodes all elements of the specified sequence and concatenates the results, or returns the first encountered error.
    * @group conv
    */
  def encodeAll(as: Iterable[A]): Attempt[BitVector] = {
    val buf = new collection.mutable.ArrayBuffer[BitVector](as.size)
    var failure: Err | Null = null
    as.foreach { a =>
      if (failure == null) {
        encode(a) match {
          case Attempt.Successful(aa) => buf += aa
          case Attempt.Failure(err)   => failure = err.pushContext(buf.size.toString)
        }
      }
    }
    if (failure == null) {
      def merge(offset: Int, size: Int): BitVector = size match {
        case 0 => BitVector.empty
        case 1 => buf(offset)
        case _ =>
          val half = size / 2
          merge(offset, half) ++ merge(offset + half, half + (if (size % 2 == 0) 0 else 1))
      }
      Attempt.successful(merge(0, buf.size))
    } else {
      Attempt.failure(failure.nn) // FIXME .nn shouldn't be necessary here b/c failure is checked for null above
    }
  }
}

/**
  * Provides functions for working with encoders.
  *
  * @groupname conv Conveniences
  * @groupprio conv 2
  *
  */
trait EncoderFunctions {

  /**
   * Encodes the specified value using the implicit `Encoder[A]`.
   * @group conv
   */
  final def encode[A](a: A)(using encA: Encoder[A]): Attempt[BitVector] =
    encA.encode(a)

  /**
    * Encodes the specified values, one after the other, to a bit vector using the specified encoders.
    * @group conv
    */
  final def encodeBoth[A, B](encA: Encoder[A], encB: Encoder[B])(a: A, b: B): Attempt[BitVector] =
    for {
      encodedA <- encA.encode(a)
      encodedB <- encB.encode(b)
    } yield encodedA ++ encodedB

  /**
    * Creates an encoder that encodes with each of the specified encoders, returning
    * the first successful result.
    * @group conv
    */
  final def choiceEncoder[A](encoders: Encoder[A]*): Encoder[A] = new Encoder[A] {
    def sizeBound = SizeBound.choice(encoders.map(_.sizeBound))
    def encode(a: A) = {
      @annotation.tailrec
      def go(rem: List[Encoder[A]], errs: List[Err]): Attempt[BitVector] = rem match {
        case Nil => Attempt.failure(Err(errs.reverse))
        case hd :: tl =>
          hd.encode(a) match {
            case res @ Attempt.Successful(_) => res
            case Attempt.Failure(err)        => go(tl, err :: errs)
          }
      }
      if (encoders.isEmpty) Attempt.failure(Err("no encoders provided"))
      else go(encoders.toList, Nil)
    }
  }
}

/**
  * Companion for [[Encoder]].
  *
  * @groupname ctor Constructors
  * @groupprio ctor 1
  *
  * @groupname inst Typeclass Instances
  * @groupprio inst 3
  */
object Encoder extends EncoderFunctions {

  inline def apply[A](using e: Encoder[A]): Encoder[A] = e

  /**
    * Creates an encoder from the specified function.
    * @group ctor
    */
  def apply[A](f: A => Attempt[BitVector]): Encoder[A] = new Encoder[A] {
    def sizeBound = SizeBound.unknown
    def encode(value: A) = f(value)
  }

  given Transform[Encoder] with {
    extension [A, B](fa: Encoder[A]) def exmap(f: A => Attempt[B], g: B => Attempt[A]): Encoder[B] =
      fa.econtramap(g)
  }

  implicit class AsSyntax[A](private val self: Encoder[A]) extends AnyVal {
    def as[B](using iso: Iso[A, B]): Encoder[B] = self.contramap(iso.from)
  }
}
