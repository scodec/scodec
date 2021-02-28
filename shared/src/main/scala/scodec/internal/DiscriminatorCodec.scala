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

import scala.reflect.ClassTag

import scodec.bits.BitVector
import DiscriminatorCodec.{Case, Prism}

/**
  * Codec that supports the binary structure `tag ++ value` where the `tag` identifies the encoding/decoding of
  * the value.
  *
  * To build an instance of this codec, call [[discriminated]] and specify the tag type via the `by` method. Then
  * call one more more of the case combinators on this class.
  *
  * The most general case combinators are `caseO` and `caseP`.
  * In addition to a tag, the `caseO` combinator is defined by providing a mapping from
  * `A` to `Option[R]`, a mapping from `R` to `A`, and a `Codec[R]`. The case is used for encoding if the
  * mapping from `A` to `Option[R]` returns a `Some` and it is used for decoding upon matching the tag value.
  * The `caseP` combinators work the same but take a `PartialFunction[A, R]` instead of an `A => Option[R]`.
  *
  * If `R` is a subtype of `A`, then the mapping from `R` to `A` can be omitted. Hence, the
  * `subcaseO` and `subcaseP` constrain `R` to being a subtype of `A` and do not take a `R => A` function.
  *
  * Finally, the least generic case combinators are the `typecase` combinators which add further constraints
  * to the `subcase*` combinators. Specifically, the typecase operators omit the `A => Option[R]` or
  * `PartialFunction[A, R]` in favor of doing subtype checks. For example, the following codec is a `Codec[AnyVal]`
  * that encodes a 0 if passed a `Boolean` and a 1 if passed an `Int`: {{{
   discriminated[AnyVal].by(uint8).typecase(0, bool).typecase(1, int32)
 }}}
  *
  * Often, the values are size-delimited -- that is, there is a `size` field after the `tag` field and before`
  * the `value` field. To support this, use the `framing` method to provide a transformation to each
  * value codec. For example, `framing(new CodecTransformation { def apply[X](c: Codec[X]) = variableSizeBytes(uint8, c) })`.
  *
  * @see [[discriminated]]
  * @group combinators
  *
  * @groupname discriminator Discriminator Support
  * @groupprio discriminator 1
  *
  * @param by codec that encodec/decodes the tag value
  * @param cases cases, ordered from highest priority to lowest priority, that handle subsets of `A`
  *
  * @define methodCaseCombinator Returns a new discriminator codec with a new case added for the specified tag.
  * @define typeR representative type that this case handles
  * @define paramTag tag value for this case
  * @define paramEncodeTag tag value to use during encoding for this case
  * @define paramDecodeTag function that determines if this case should be used for decoding given a decoded tag
  * @define paramToRep function used during encoding that converts an `A` to an `Option[R]`
  * @define paramToRepPartial partial function from `A` to `R` used during encoding
  * @define paramFromRep function used during decoding that converts an `R` to an `A`
  * @define paramCr codec that encodes/decodes `R`s
  */
final class DiscriminatorCodec[A, B] private[scodec] (
    by: Codec[B],
    cases: Vector[Case[A, B, Any]],
    framing: [x] => Codec[x] => Codec[x]
) extends Codec[A]
    with KnownDiscriminatorType[B]:

  /**
    * $methodCaseCombinator
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRep
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def caseO[R](
      tag: B
  )(toRep: A => Option[R])(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    appendCase(Case(tag, Prism(toRep, fromRep, cr)))

  /**
    * $methodCaseCombinator
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRepPartial
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def caseP[R](
      tag: B
  )(toRep: PartialFunction[A, R])(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    appendCase(Case(tag, Prism(toRep.lift, fromRep, cr)))

  /**
    * $methodCaseCombinator
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRep
    * @param cr $paramCr
    * @group discriminator
    */
  def subcaseO[R <: A](tag: B)(toRep: A => Option[R])(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseO(tag)(toRep)((r: R) => r)(cr)

  /**
    * $methodCaseCombinator
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRepPartial
    * @param cr $paramCr
    * @group discriminator
    */
  def subcaseP[R <: A](
      tag: B
  )(toRep: PartialFunction[A, R])(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseP(tag)(toRep)((r: R) => r)(cr)

  /**
    * $methodCaseCombinator
    *
    * Note: when encoding a value of `A`, this combinator compares the runtime class of that value to the
    * runtime class of the supplied `ClassTag[R]`. As such, the *erased* type of `A` is used and hence,
    * this operation is not safe to use with parameterized representation types.
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param cr $paramCr
    * @group discriminator
    */
  def typecase[R <: A: ClassTag](tag: B, cr: Codec[R]): DiscriminatorCodec[A, B] =
    subcaseO(tag)(a => if matchesClass[R](a) then Some(a.asInstanceOf[R]) else None)(cr)

  private def matchesClass[R](a: A)(using ctr: ClassTag[R]) =
    val clazz = ctr match
      case ClassTag.Byte    => classOf[java.lang.Byte]
      case ClassTag.Char    => classOf[java.lang.Character]
      case ClassTag.Short   => classOf[java.lang.Short]
      case ClassTag.Int     => classOf[java.lang.Integer]
      case ClassTag.Long    => classOf[java.lang.Long]
      case ClassTag.Float   => classOf[java.lang.Float]
      case ClassTag.Double  => classOf[java.lang.Double]
      case ClassTag.Boolean => classOf[java.lang.Boolean]
      case ct               => ct.runtimeClass
    clazz.isAssignableFrom(a.getClass)

  def singleton[R <: A](tag: B, value: R): DiscriminatorCodec[A, B] =
    subcaseP(tag) { case v if value == v => v }(codecs.provide(value))

  private def appendCase[R](c: Case[A, B, R]): DiscriminatorCodec[A, B] =
    new DiscriminatorCodec[A, B](by, cases :+ c.asInstanceOf[Case[A, B, Any]], framing)

  private val matcher: B => Attempt[Case[A, B, Any]] =
    def errOrCase(b: B, opt: Option[Case[A, B, Any]]) =
      Attempt.fromOption(opt, new UnknownDiscriminator(b))
    // we reverse the cases so earlier cases 'win' in event of overlap
    val tbl = cases.reverse.map(kase => kase.tag -> kase).toMap
    b => errOrCase(b, tbl.get(b))

  /**
    * Replaces the current framing logic with the specified codec transformation.
    *
    * Every representative codec is wrapped with the framing logic when encoding/decoding.
    *
    * @param framing new framing logic
    * @group discriminator
    */
  def framing(framing: [x] => Codec[x] => Codec[x]): DiscriminatorCodec[A, B] =
    new DiscriminatorCodec[A, B](by, cases, framing)

  def sizeBound =
    by.sizeBound + SizeBound.choice(cases.map(c => framing(c.prism.repCodec).sizeBound))

  def encode(a: A) =
    val itr = cases.iterator
      .flatMap { k =>
        k.prism
          .preview(a)
          .map { r =>
            by.encode(k.tag)
              .flatMap { bits =>
                framing(k.prism.repCodec).encode(r).map(bits ++ _)
              }
          }
          .map(List(_))
          .getOrElse(List())
      }
    if itr.hasNext then itr.next
    else Attempt.failure(new Err.MatchingDiscriminatorNotFound(a))

  def decode(bits: BitVector) =
    (for
      b <- by
      k <- Decoder.liftAttempt(matcher(b))
      r <- framing(k.prism.repCodec)
    yield k.prism.review(r)).decode(bits)

  override def toString = s"discriminated($by)"

/** Companion for [[Discriminator]]. */
private[scodec] object DiscriminatorCodec:

  /** Provides an injection between `A` and `R` and a `Codec[R]`. */
  private[scodec] case class Prism[A, R](
      preview: A => Option[R],
      review: R => A,
      repCodec: Codec[R]
  )

  /**
    * Maps a discrimination tag to a prism that supports encoding/decoding a value of type `A`.
    */
  private[scodec] case class Case[A, B, R](tag: B, prism: Prism[A, R])
