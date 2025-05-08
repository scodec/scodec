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

import scala.deriving.*
import scala.compiletime.*

import scodec.bits.{BitVector, ByteVector}

/** Supports encoding a value of type `A` to a `BitVector` and decoding a `BitVector` to a value of `A`.
  *
  * Not every value of `A` can be encoded to a bit vector and similarly, not every bit vector can be decoded to a value
  * of type `A`. Hence, both encode and decode return either an error or the result. Furthermore, decode returns the
  * remaining bits in the bit vector that it did not use in decoding.
  *
  * There are various ways to create instances of `Codec`. The trait can be implemented directly or one of the
  * constructor methods in the companion can be used (e.g., `apply`). Most of the methods on `Codec`
  * create return a new codec that has been transformed in some way. For example, the [[xmap]] method
  * converts a `Codec[A]` to a `Codec[B]` given two functions, `A => B` and `B => A`.
  *
  * One of the simplest transformation methods is `def withContext(context: String): Codec[A]`, which
  * pushes the specified context string in to any errors (i.e., `Err`s) returned from encode or decode.
  *
  * See the methods on this trait for additional transformation types.
  *
  * See the [[codecs]] package object for pre-defined codecs for many common data types and combinators for building larger
  * codecs out of smaller ones.
  *
  * ## Tuple Codecs
  *
  * The `::` operator supports combining a `Codec[A]` and a `Codec[B]` in to a `Codec[(A, B)]`.
  *
  * For example: {{{
  *   val codec: Codec[(Int, Int, Int)] = uint8 :: uint8 :: uint8
  * }}}
  *
  * There are various methods on `Codec` that only work on `Codec[A]` for some `A <: Tuple`. Besides the aforementioned
  * `::` method, they include methods like `++`, `flatPrepend`, `flatConcat`, etc. One particularly useful method is
  * `dropUnits`, which removes any `Unit` values from the tuple.
  *
  * Given a `Codec[(X0, X1, ..., Xn)]` and a case class with types `X0` to `Xn` in the same order,
  * the codec can be turned in to a case class codec via the `as` method. For example:
  * {{{
  *  case class Point(x: Int, y: Int, z: Int)
  *  val threeInts: Codec[(Int, Int, Int)] = uint8 :: uint8 :: uint8
  *  val point: Codec[Point] = threeInts.as[Point]
  * }}}
  *
  * ### flatZip
  *
  * Sometimes when combining codecs, a latter codec depends on a formerly decoded value.
  * The `flatZip` method is important in these types of situations -- it represents a dependency between
  * the left hand side and right hand side. Its signature is `def flatZip[B](f: A => Codec[B]): Codec[(A, B)]`.
  * This is similar to `flatMap` except the return type is `Codec[(A, B)]` instead of `Decoder[B]`.
  *
  * Consider a binary format of an 8-bit unsigned integer indicating the number of bytes following it.
  * To implement this with `flatZip`, we could write: {{{
  *  val x: Codec[(Int, ByteVector)] = uint8.flatZip { numBytes => bytes(numBytes) }
  *  val y: Codec[ByteVector] = x.xmap[ByteVector]({ case (_, bv) => bv }, bv => (bv.size, bv))
  * }}}
  * In this example, `x` is a `Codec[(Int, ByteVector)]` but we do not need the size directly in the model
  * because it is redundant with the size stored in the `ByteVector`. Hence, we remove the `Int` by
  * `xmap`-ping over `x`. The notion of removing redundant data from models comes up frequently.
  * Note: there is a combinator that expresses this pattern more succinctly -- `variableSizeBytes(uint8, bytes)`.
  *
  * ### flatPrepend
  *
  * When the function passed to `flatZip` returns a `Codec[B]` where `B <: Tuple`, you end up creating
  * right nested tuples instead of a extending the arity of a single tuple. To do the latter, there's
  * `flatPrepend`. It has the signature: {{{
  *  def flatPrepend[B <: Tuple](f: A => Codec[B]): Codec[A *: B]
  * }}}
  * It forms a codec of `A` consed on to `B` when called on a `Codec[A]` and passed a function `A => Codec[B]`.
  * Note that the specified function must return a tuple codec. Implementing our example from earlier
  * using `flatPrepend`: {{{
  *  val x: Codec[(Int, ByteVector)] = uint8.flatPrepend { numBytes => bytes(numBytes).tuple }
  * }}}
  * In this example, `bytes(numBytes)` returns a `Codec[ByteVector]` so we called `.tuple` on it to lift it
  * in to a `Codec[ByteVector *: Unit]`.
  *
  * There are similar methods for flat appending and flat concating.
  *
  * ## Derived Codecs
  *
  * Codecs for case classes and sealed class hierarchies can often be automatically derived.
  *
  * Consider this example: {{{
  *  case class Point(x: Int, y: Int, z: Int) derives Codec
  *  Codec[Point].encode(Point(1, 2, 3))
  * }}}
  * In this example, no explicit codec was defined for `Point` and instead, one was derived as a result
  * of the `derives Codec` clause. Derivation of a codec for a case class requires each element of the case class to
  * have a given codec of the corresponding type. In this case, each element was an `Int` and there is
  * a `Codec[Int]` given in the companion of `Codec`.
  *
  * Derived codecs include the name of each element in any errors produced when encoding/decoding the element.
  *
  * This works similarly for ADTs / sealed class hierarchies. The binary form is represented as a single
  * unsigned 8-bit integer representing the ordinal of the sum, followed by the derived form of the product.
  *
  * Full examples are available in the test directory of this project.
  */
trait Codec[A] extends Encoder[A], Decoder[A]:
  self =>

  /** Transforms this codec to a `Codec[B]` if `A` is isomorphic to `B`.
    *
    * This is most commonly used to convert a tuple codec to a case class:
    * @example {{{
    * case class Point(x: Int, y: Int, z: Int)
    * val c: Codec[(Int, Int, Int)] = int8 :: int8 :: int8
    * val p: Codec[Point] = c.as[Point]
    * }}}
    */
  def as[B](using iso: Iso[A, B]): Codec[B] = xmap(iso.to, iso.from)

  /** Transforms using two functions, `A => Attempt[B]` and `B => Attempt[A]`.
    */
  final def exmap[B](f: A => Attempt[B], g: B => Attempt[A]): Codec[B] = new Codec[B]:
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = self.econtramap(g).encode(b)
    def decode(buffer: BitVector) = self.emap(f).decode(buffer)

  /** Transforms using the isomorphism described by two functions, `A => B` and `B => A`.
    */
  final def xmap[B](f: A => B, g: B => A): Codec[B] = new Codec[B]:
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = self.encode(g(b))
    def decode(buffer: BitVector) = self.decode(buffer).map(_.map(f))

  /** Lifts this codec in to a codec of a singleton tuple.
    */
  final def tuple: Codec[A *: EmptyTuple] = xmap(_ *: Tuple(), _.head)

  @deprecated("Use .tuple instead", "2.0.0")
  final def hlist: Codec[A *: EmptyTuple] = tuple

  /** Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
    * decodes a unit followed by a `B` and discards the decoded unit.
    */
  final def dropLeft[B](codecB: Codec[B])(using ev: Unit =:= A): Codec[B] =
    (this :: codecB).xmap[B]((_, b) => b, b => (ev(()), b))

  /** Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
    * decodes a unit followed by a `B` and discards the decoded unit.
    *
    * Operator alias of [[dropLeft]].
    */
  final def ~>[B](codecB: Codec[B])(using Unit =:= A): Codec[B] = dropLeft(codecB)

  /** Assuming `B` is `Unit`, creates a `Codec[A]` that: encodes the `A` followed by a unit;
    * decodes an `A` followed by a unit and discards the decoded unit.
    */
  final def dropRight[B](codecB: Codec[B])(using ev: Unit =:= B): Codec[A] =
    (this :: codecB).xmap[A]((a, _) => a, a => (a, ev(())))

  /** Assuming `B` is `Unit`, creates a `Codec[A]` that: encodes the `A` followed by a unit;
    * decodes an `A` followed by a unit and discards the decoded unit.
    *
    * Operator alias of [[dropRight]].
    */
  final def <~[B](codecB: Codec[B])(using Unit =:= B): Codec[A] = dropRight(codecB)

  /** Converts this to a `Codec[Unit]` that encodes using the specified zero value and
    * decodes a unit value when this codec decodes an `A` successfully.
    */
  final def unit(zero: A): Codec[Unit] = xmap[Unit](_ => (), _ => zero)

  /** Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
    */
  final def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)]:
    def sizeBound: SizeBound = self.sizeBound.atLeast
    override def encode(t: (A, B)) = Codec.encodeBoth(self, f(t._1))(t._1, t._2)
    override def decode(buffer: BitVector) =
      (for
        a <- self
        b <- f(a)
      yield (a, b)).decode(buffer)

  /** Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
    * Operator alias for [[flatZip]].
    */
  final def >>~[B](f: A => Codec[B]): Codec[(A, B)] = flatZip(f)

  /** Similar to `flatZip` except the `A` type is not visible in the resulting type -- the binary
    * effects of the `Codec[A]` still occur though.
    *
    * Example usage: {{{
    *     case class Flags(x: Boolean, y: Boolean, z: Boolean)
    *     (bool :: bool :: bool :: ignore(5)).consume { flgs =>
    *       conditional(flgs.x, uint8) :: conditional(flgs.y, uint8) :: conditional(flgs.z, uint8)
    *     } {
    *       case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
    *     }
    *   }}}
    */
  final def consume[B](f: A => Codec[B])(g: B => A): Codec[B] = new Codec[B]:
    def sizeBound = self.sizeBound.atLeast
    def encode(b: B) =
      val a = g(b)
      for
        encA <- self.encode(a)
        encB <- f(a).encode(b)
      yield encA ++ encB
    def decode(bv: BitVector) =
      (for
        a <- self
        b <- f(a)
      yield b).decode(bv)

  final override def complete: Codec[A] = Codec(this, super.complete)

  final override def compact: Codec[A] = Codec(super.compact, this)

  /** Safely lifts this codec to a codec of a supertype.
    *
    * When a subtype of `B` that is not a subtype of `A` is passed to encode,
    * an encoding error is returned.
    */
  final def upcast[B >: A](using reflect.TypeTest[B, A]): Codec[B] = new Codec[B]:
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = b match
      case a: A => self.encode(a)
      case _    => Attempt.failure(Err("not a value of desired type"))
    def decode(bv: BitVector) = self.decode(bv)
    override def toString = self.toString

  /** Safely lifts this codec to a codec of a subtype.
    *
    * When a supertype of `B` that is not a supertype of `A` is decoded,
    * an decoding error is returned.
    */
  final def downcast[B <: A](using reflect.TypeTest[A, B]): Codec[B] = new Codec[B]:
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = self.encode(b)
    def decode(bv: BitVector) = self.decode(bv).flatMap { result =>
      result.value match
        case b: B => Attempt.successful(DecodeResult(b, result.remainder))
        case _    => Attempt.failure(Err("not a value of desired type"))
    }
    override def toString = self.toString

  /** Creates a new codec that is functionally equivalent to this codec but pushes the specified
    * context string in to any errors returned from encode or decode.
    */
  final def withContext(context: String): Codec[A] = new Codec[A]:
    def sizeBound: SizeBound = self.sizeBound
    override def encode(a: A) = self.encode(a).mapErr(_.pushContext(context))
    override def decode(buffer: BitVector) = self.decode(buffer).mapErr(_.pushContext(context))
    override def toString = s"$context($self)"

  /** Creates a new codec that is functionally equivalent to this codec but returns the specified string from `toString`.
    */
  final def withToString(str: => String): Codec[A] = new Codec[A]:
    override def sizeBound: SizeBound = self.sizeBound
    override def encode(a: A) = self.encode(a)
    override def decode(buffer: BitVector) = self.decode(buffer)
    override def toString = str

  override def decodeOnly[AA >: A]: Codec[AA] =
    val sup = super.decodeOnly[AA]
    new Codec[AA]:
      def sizeBound = self.sizeBound
      def encode(a: AA) = sup.encode(a)
      def decode(bits: BitVector) = sup.decode(bits)

/** Companion for [[Codec]].
  */
object Codec extends EncoderFunctions, DecoderFunctions:

  inline def apply[A](using c: Codec[A]): Codec[A] = c

  /** Creates a codec from encoder and decoder functions.
    */
  def apply[A](
      encoder: A => Attempt[BitVector],
      decoder: BitVector => Attempt[DecodeResult[A]]
  ): Codec[A] = new Codec[A]:
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(a: A) = encoder(a)
    override def decode(bits: BitVector) = decoder(bits)

  /** Creates a codec from an encoder and a decoder.
    */
  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A]:
    override def sizeBound: SizeBound = encoder.sizeBound
    override def encode(a: A) = encoder.encode(a)
    override def decode(bits: BitVector) = decoder.decode(bits)

  /** Provides a `Codec[A]` that delegates to a lazily evaluated `Codec[A]`.
    * Typically used to consruct codecs for recursive structures.
    */
  def lazily[A](codec: => Codec[A]): Codec[A] = new Codec[A]:
    @annotation.threadUnsafe
    lazy val c: Codec[A] = codec
    def sizeBound = c.sizeBound
    def encode(a: A) = c.encode(a)
    def decode(b: BitVector) = c.decode(b)
    override def toString = s"lazily($c)"

  extension [A <: Tuple](codecA: Codec[A])
    inline def dropUnits: Codec[DropUnits[A]] =
      codecA.xmap(a => DropUnits.drop(a), b => DropUnits.insert(b))

  /** When called on a `Codec[A]` for some `A <: Tuple`, returns a new codec that encodes/decodes
    * the tuple `A` followed by the value `B`, where the latter is encoded/decoded with the codec
    * returned from applying `A` to `f`.
    */
  extension [A <: Tuple, B](codecA: Codec[A])
    inline def flatAppend(f: A => Codec[B]): Codec[Tuple.Concat[A, B *: EmptyTuple]] =
      inlineImplementations.FlatAppendCodec(constValue[Tuple.Size[A]], codecA, f)

  /** Builds a `Codec[A *: B]` from a `Codec[A]` and a `Codec[B]` where `B` is a tuple type.
    * That is, this operator is a codec-level tuple prepend operation.
    * @param codec codec to prepend
    */
  extension [A, B <: Tuple](codecA: Codec[A])
    def ::(codecB: Codec[B]): Codec[A *: B] =
      inlineImplementations.ConsCodec(codecA, codecB)

  /** `codecB :+ codecA` returns a new codec that encodes/decodes the tuple `B` followed by an `A`.
    * That is, this operator is a codec-level tuple append operation.
    */
  extension [A, B <: Tuple](codecB: Codec[B])
    inline def :+(codecA: Codec[A]): Codec[Tuple.Concat[B, A *: EmptyTuple]] =
      codecB ++ codecA.tuple

  /** Builds a `Codec[A ++ B]` from a `Codec[A]` and a `Codec[B]` where `A` and `B` are tuples.
    * That is, this operator is a codec-level tuple concat operation.
    * @param codecA codec to concat
    */
  extension [A <: Tuple, B <: Tuple](codecA: Codec[A])
    inline def ++(codecB: Codec[B]): Codec[Tuple.Concat[A, B]] =
      inlineImplementations.ConcatCodec(constValue[Tuple.Size[A]], codecA, codecB)

  /** When called on a `Codec[A]` for some `A <: Tuple`, returns a new codec that encodes/decodes
    * the tuple `A` followed by the tuple `B`, where the latter is encoded/decoded with the codec
    * returned from applying `A` to `f`.
    */
  extension [A <: Tuple, B <: Tuple](codecA: Codec[A])
    inline def flatConcat(f: A => Codec[B]): Codec[Tuple.Concat[A, B]] =
      inlineImplementations.FlatConcatCodec(constValue[Tuple.Size[A]], codecA, f)

  /** When called on a `Codec[A]` where `A` is not a tuple, creates a new codec that encodes/decodes a tuple of `(B, A)`.
    * For example, {{{uint8 :: utf8}}} has type `Codec[(Int, Int)]`.
    */
  extension [A, B](a: Codec[A])
    def ::(b: Codec[B])(using DummyImplicit): Codec[(A, B)] =
      inlineImplementations.PairCodec(a, b)

  /** Creates a new codec that encodes/decodes a tuple of `A :: B` given a function `A => Codec[B]`.
    * This allows later parts of a tuple codec to be dependent on earlier values.
    */
  extension [A, B <: Tuple](codecA: Codec[A])
    def flatPrepend(f: A => Codec[B]): Codec[A *: B] =
      new Codec[A *: B]:
        def sizeBound = codecA.sizeBound.atLeast
        def encode(ab: A *: B) = encodeBoth(codecA, f(ab.head))(ab.head, ab.tail)
        def decode(b: BitVector) =
          (for
            a <- codecA
            l <- f(a)
          yield a *: l).decode(b)
        override def toString = s"flatPrepend($codecA, $f)"

  /** Constructs a `Codec[(A, B, ..., N)]` from a tuple `(Codec[A], Codec[B], ..., Codec[N])`.
    */
  def fromTuple[A <: Tuple: Tuple.IsMappedBy[Codec]](a: A): Codec[Tuple.InverseMap[A, Codec]] =
    def go[X <: Tuple](x: X): Codec[? <: Tuple] = x match
      case hd *: tl   => hd.asInstanceOf[Codec[?]] :: go(tl)
      case EmptyTuple => codecs.provide(EmptyTuple)
    go(a).asInstanceOf[Codec[Tuple.InverseMap[A, Codec]]]

  inline given derivedTuple[A <: Tuple]: Codec[A] =
    lazy val componentCodecs =
      summonAll[Tuple.Map[A, Codec]].toList.asInstanceOf[List[Codec[Any]]].toArray
    inlineImplementations.TupleCodec(componentCodecs)

  inline def derived[A](using m: Mirror.Of[A]): Codec[A] =
    lazy val componentCodecs =
      val unlabelled =
        summonCodecInstances[A, m.MirroredElemTypes].asInstanceOf[List[Codec[Any]]].toArray
      val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
      unlabelled
        .zip(labels)
        .map: (codec, label) =>
          codec.withContext(label)
    inline m match
      case p: Mirror.ProductOf[A] =>
        inlineImplementations
          .TupleCodec[p.MirroredElemTypes](componentCodecs)
          .xmap(
            met => p.fromTuple(met),
            a => Tuple.fromProduct(a.asInstanceOf[Product]).asInstanceOf[p.MirroredElemTypes]
          )
      case s: Mirror.SumOf[A] =>
        inlineImplementations.SumCodec(s, componentCodecs)

  class SummonCodecStep[Base] extends InlineFoldR.Step[[T <: Tuple] =>> List[Codec[?]]]:
    inline def apply[Elem, T <: Tuple](acc: List[Codec[?]]): List[Codec[?]] =
      deriveOrSummonCodec[Base, Elem] :: acc

  private inline def summonCodecInstances[T, Elems <: Tuple]: List[Codec[?]] =
    InlineFoldR.fold[[T <: Tuple] =>> List[Codec[?]], Elems](Nil, SummonCodecStep[T])

  // From https://github.com/scala/scala3/blob/ebbd685cfb02e1935afb9b4fd568643315825c57/docs/_docs/reference/contextual/derivation.md
  private inline def deriveOrSummonCodec[T, Elem]: Codec[Elem] =
    inline erasedValue[Elem & Matchable] match
      case _: T => deriveCodecRecursive[T, Elem]
      case _    => summonInline[Codec[Elem]]

  // From https://github.com/scala/scala3/blob/ebbd685cfb02e1935afb9b4fd568643315825c57/docs/_docs/reference/contextual/derivation.md
  private inline def deriveCodecRecursive[T, Elem]: Codec[Elem] =
    inline erasedValue[T & Matchable] match
      case _: Elem => error("infinite recursive derivation")
      case _       => Codec.derived[Elem](using summonInline[Mirror.Of[Elem]])

  /** Implementations for various inline defs. Do not use directly! No binary compatibility provided for classes in this package. */
  object inlineImplementations:

    final class ConcatCodec[A <: Tuple, B <: Tuple](
        sizeA: Tuple.Size[A],
        codecA: Codec[A],
        codecB: Codec[B]
    ) extends Codec[Tuple.Concat[A, B]]:
      def sizeBound = codecA.sizeBound + codecB.sizeBound
      def encode(ab: Tuple.Concat[A, B]) =
        val (prefix, suffix) = ab.splitAt(sizeA)
        encodeBoth(codecA, codecB)(prefix.asInstanceOf[A], suffix.asInstanceOf[B])
      def decode(bv: BitVector) =
        decodeBoth(codecA, codecB)(bv).map(
          _.map((a: A, b: B) => a ++ b)
        )
      override def toString = s"$codecA ++ $codecB"

    final class ConsCodec[A, B <: Tuple](codecA: Codec[A], codecB: Codec[B]) extends Codec[A *: B]:
      def sizeBound = codecA.sizeBound + codecB.sizeBound
      def encode(ab: A *: B) = encodeBoth(codecA, codecB)(ab.head, ab.tail)
      def decode(bv: BitVector) = decodeBoth(codecA, codecB)(bv).map(_.map(_ *: _))
      override def toString = s"$codecA :: $codecB"

    final class FlatAppendCodec[A <: Tuple, B](
        sizeA: Tuple.Size[A],
        codecA: Codec[A],
        f: A => Codec[B]
    ) extends Codec[Tuple.Concat[A, B *: EmptyTuple]]:
      def sizeBound = codecA.sizeBound.atLeast
      def encode(ab: Tuple.Concat[A, B *: EmptyTuple]) =
        val (a, b) = ab.splitAt(sizeA).asInstanceOf[(A, B *: EmptyTuple)]
        encodeBoth(codecA, f(a))(a, b.head)
      def decode(bv: BitVector) =
        codecA
          .decode(bv)
          .flatMap:
            case DecodeResult(a, rem) =>
              f(a)
                .decode(rem)
                .map(_.map(b => a ++ (b *: EmptyTuple)))
      override def toString = s"$codecA.flatAppend(<f>)"

    final class FlatConcatCodec[A <: Tuple, B <: Tuple](
        sizeA: Tuple.Size[A],
        codecA: Codec[A],
        f: A => Codec[B]
    ) extends Codec[Tuple.Concat[A, B]]:
      def sizeBound = codecA.sizeBound.atLeast
      def encode(ab: Tuple.Concat[A, B]) =
        val (a, b) = ab.splitAt(sizeA).asInstanceOf[(A, B)]
        encodeBoth(codecA, f(a))(a, b)
      def decode(bv: BitVector) =
        codecA
          .decode(bv)
          .flatMap:
            case DecodeResult(a, rem) =>
              f(a).decode(rem).map(_.map(b => a ++ b))
      override def toString = s"$codecA.flatConcat(<f>)"

    final class PairCodec[A, B](a: Codec[A], b: Codec[B]) extends Codec[(A, B)]:
      def sizeBound = a.sizeBound + b.sizeBound
      def encode(ab: (A, B)) = Codec.encodeBoth(a, b)(ab._1, ab._2)
      def decode(bv: BitVector) = Codec.decodeBoth(a, b)(bv)
      override def toString = s"$a :: $b"

    final class TupleCodec[A <: Tuple](componentCodecs0: => Array[Codec[Any]]) extends Codec[A]:
      private lazy val componentCodecs: Array[Codec[Any]] = componentCodecs0

      def sizeBound = componentCodecs.foldLeft(SizeBound.exact(0L))(_ + _.sizeBound)

      def encode(t: A) =
        val components = t.toArray
        var i = 0
        var result: Attempt[BitVector] = null
        var acc: BitVector = BitVector.empty
        while i < componentCodecs.size && (result eq null) do
          val codec = componentCodecs(i)
          codec.encode(components(i)) match
            case Attempt.Successful(enc) =>
              acc = acc ++ enc
              i += 1
            case f @ Attempt.Failure(_) =>
              result = f
        if result ne null then result else Attempt.Successful(acc)

      def decode(b: BitVector) =
        var remaining = b
        val components = new Array[Any](componentCodecs.size)
        var i = 0
        var result: Attempt[DecodeResult[A]] = null
        while i < componentCodecs.length && (result eq null) do
          val codec = componentCodecs(i)
          codec.decode(remaining) match
            case Attempt.Successful(DecodeResult(c, rem)) =>
              components(i) = c
              i += 1
              remaining = rem
            case f @ Attempt.Failure(_) =>
              result = f
        if result ne null then result
        else
          Attempt.Successful(DecodeResult(Tuple.fromArray(components).asInstanceOf[A], remaining))

      override def toString = "TupleCodec"

    final class SumCodec[A](mirror: Mirror.SumOf[A], caseCodecs0: => Array[Codec[Any]])
        extends Codec[A]:
      private lazy val caseCodecs: Array[Codec[Any]] = caseCodecs0

      def sizeBound = caseCodecs.foldLeft(codecs.uint8.sizeBound)(_ + _.sizeBound)

      def encode(a: A) =
        val ordinal = mirror.ordinal(a)
        Encoder.encodeBoth(codecs.uint8, caseCodecs(ordinal))(ordinal, a)

      def decode(bits: BitVector) =
        codecs.uint8
          .flatMap(ordinal => caseCodecs(ordinal))
          .decode(bits)
          .asInstanceOf[Attempt[DecodeResult[A]]]

      override def toString = "SumCodec"

  given Codec[Byte] = codecs.byte
  given Codec[Short] = codecs.short16
  given Codec[Int] = codecs.int32
  given Codec[Long] = codecs.int64
  given Codec[Float] = codecs.float
  given Codec[Double] = codecs.double
  given Codec[String] = codecs.utf8_32
  given Codec[Boolean] = codecs.bool(8)
  given Codec[BitVector] = codecs.variableSizeBitsLong(codecs.int64, codecs.bits)
  given Codec[ByteVector] = codecs.variableSizeBytesLong(codecs.int64, codecs.bytes)
  given Codec[java.util.UUID] = codecs.uuid

  given [A](using ccount: Codec[Int], ca: Codec[A]): Codec[List[A]] = codecs.listOfN(ccount, ca)
  given [A](using ccount: Codec[Int], ca: Codec[A]): Codec[Vector[A]] = codecs.vectorOfN(ccount, ca)
  given [A](using cguard: Codec[Boolean], ca: Codec[A]): Codec[Option[A]] =
    codecs.optional(cguard, ca)

  given Transform[Codec] with
    extension [A, B](fa: Codec[A])
      def exmap(f: A => Attempt[B], g: B => Attempt[A]): Codec[B] =
        fa.exmap(f, g)
