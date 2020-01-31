package scodec
package codecs

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
  * Each of the case combinators provides two forms, one that defines a case with a single tag value and another,
  * overloaded form that defines a case with a tag value to use in encoding and a predicate on tag value to use
  * in decoding.
  *
  * The most general case combinators are `caseO` and `caseP` (and their operator equivalents, `?` and `|`).
  * In addition to a tag or tag/predicate pair, the `caseO` combinators are defined by providing a mapping from
  * `A` to `Option[R]`, a mapping from `R` to `A`, and a `Codec[R]`. The case is used for encoding if the
  * mapping from `A` to `Option[R]` returns a `Some` and it is used for decoding upon matching the tag value.
  * The `caseP` combinators work the same but take a `PartialFunction[A, R]` instead of an `A => Option[R]`.
  *
  * If `R` is a subtype of `A`, then the mapping from `R` to `A` can be omitted. Hence, the
  * `subcaseO` and `subcaseP` (and the operator equivalents, `/` and `\`) constrain `R` to being a subtype
  * of `A` and do not take a `R => A` function.
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
final class DiscriminatorCodec[A, B] private[codecs] (
    by: Codec[B],
    cases: Vector[Case[A, B, Any]],
    framing: CodecTransformation
) extends Codec[A]
    with KnownDiscriminatorType[B] {

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
    appendCase(Case(Left(tag), Prism(toRep, fromRep, cr)))

  /**
    * $methodCaseCombinator
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRep
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def caseO[R](encodeTag: B, decodeTag: B => Boolean)(
      toRep: A => Option[R]
  )(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    appendCase(Case(Right(encodeTag -> decodeTag), Prism(toRep, fromRep, cr)))

  /**
    * $methodCaseCombinator
    * Operator alias for `caseO`.
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRep
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def ?[R](tag: B)(toRep: A => Option[R])(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseO(tag)(toRep)(fromRep)(cr)

  /**
    * $methodCaseCombinator
    * Operator alias for `caseO`.
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRep
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def ?[R](encodeTag: B, decodeTag: B => Boolean)(
      toRep: A => Option[R]
  )(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseO(encodeTag, decodeTag)(toRep)(fromRep)(cr)

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
    appendCase(Case(Left(tag), Prism(toRep.lift, fromRep, cr)))

  /**
    * $methodCaseCombinator
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRepPartial
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def caseP[R](encodeTag: B, decodeTag: B => Boolean)(
      toRep: PartialFunction[A, R]
  )(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    appendCase(Case(Right(encodeTag -> decodeTag), Prism(toRep.lift, fromRep, cr)))

  /**
    * $methodCaseCombinator
    * Operator alias for `caseP`.
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRepPartial
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def |[R](
      tag: B
  )(toRep: PartialFunction[A, R])(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseP(tag)(toRep)(fromRep)(cr)

  /**
    * $methodCaseCombinator
    * Operator alias for `caseP`.
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRepPartial
    * @param fromRep $paramFromRep
    * @param cr $paramCr
    * @group discriminator
    */
  def |[R](encodeTag: B, decodeTag: B => Boolean)(
      toRep: PartialFunction[A, R]
  )(fromRep: R => A)(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseP(encodeTag, decodeTag)(toRep)(fromRep)(cr)

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
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRep
    * @param cr $paramCr
    * @group discriminator
    */
  def subcaseO[R <: A](encodeTag: B, decodeTag: B => Boolean)(
      toRep: A => Option[R]
  )(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseO(encodeTag, decodeTag)(toRep)((r: R) => r)(cr)

  /**
    * $methodCaseCombinator
    * Operator alias for `subcaseO`.
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRep
    * @param cr $paramCr
    * @group discriminator
    */
  def /[R <: A](tag: B)(toRep: A => Option[R])(cr: Codec[R]): DiscriminatorCodec[A, B] =
    subcaseO(tag)(toRep)(cr)

  /**
    * $methodCaseCombinator
    * Operator alias for `subcaseO`.
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRep
    * @param cr $paramCr
    * @group discriminator
    */
  def /[R <: A](encodeTag: B, decodeTag: B => Boolean)(
      toRep: A => Option[R]
  )(cr: Codec[R]): DiscriminatorCodec[A, B] =
    subcaseO(encodeTag, decodeTag)(toRep)(cr)

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
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRepPartial
    * @param cr $paramCr
    * @group discriminator
    */
  def subcaseP[R <: A](encodeTag: B, decodeTag: B => Boolean)(
      toRep: PartialFunction[A, R]
  )(cr: Codec[R]): DiscriminatorCodec[A, B] =
    caseP(encodeTag, decodeTag)(toRep)((r: R) => r)(cr)

  /**
    * $methodCaseCombinator
    * Operator alias for `subcaseP`.
    *
    * @tparam R $typeR
    * @param tag $paramTag
    * @param toRep $paramToRepPartial
    * @param cr $paramCr
    * @group discriminator
    */
  def \[R <: A](tag: B)(toRep: PartialFunction[A, R])(cr: Codec[R]): DiscriminatorCodec[A, B] =
    subcaseP(tag)(toRep)(cr)

  /**
    * $methodCaseCombinator
    * Operator alias for `subcaseP`.
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param toRep $paramToRepPartial
    * @param cr $paramCr
    * @group discriminator
    */
  def \[R <: A](encodeTag: B, decodeTag: B => Boolean)(
      toRep: PartialFunction[A, R]
  )(cr: Codec[R]): DiscriminatorCodec[A, B] =
    subcaseP(encodeTag, decodeTag)(toRep)(cr)

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
    subcaseO(tag)(a => if (matchesClass[R](a)) Some(a.asInstanceOf[R]) else None)(cr)

  /**
    * $methodCaseCombinator
    *
    * Note: when encoding a value of `A`, this combinator compares the runtime class of that value to the
    * runtime class of the supplied `ClassTag[R]`. As such, the *erased* type of `A` is used and hence,
    * this operation is not safe to use with parameterized representation types.
    *
    * @tparam R $typeR
    * @param encodeTag $paramEncodeTag
    * @param decodeTag $paramDecodeTag
    * @param cr $paramCr
    * @group discriminator
    */
  def typecase[R <: A: ClassTag](
      encodeTag: B,
      decodeTag: B => Boolean,
      cr: Codec[R]
  ): DiscriminatorCodec[A, B] =
    subcaseO(encodeTag, decodeTag)(a => if (matchesClass[R](a)) Some(a.asInstanceOf[R]) else None)(
      cr
    )

  private def matchesClass[R](a: A)(implicit ctr: ClassTag[R]) = {
    val clazz = ctr match {
      case ClassTag.Byte    => classOf[java.lang.Byte]
      case ClassTag.Char    => classOf[java.lang.Character]
      case ClassTag.Short   => classOf[java.lang.Short]
      case ClassTag.Int     => classOf[java.lang.Integer]
      case ClassTag.Long    => classOf[java.lang.Long]
      case ClassTag.Float   => classOf[java.lang.Float]
      case ClassTag.Double  => classOf[java.lang.Double]
      case ClassTag.Boolean => classOf[java.lang.Boolean]
      case ct               => ct.runtimeClass
    }
    clazz.isAssignableFrom(a.getClass)
  }

  private def appendCase[R](c: Case[A, B, R]): DiscriminatorCodec[A, B] =
    new DiscriminatorCodec[A, B](by, cases :+ c.asInstanceOf[Case[A, B, Any]], framing)

  private val matcher: B => Attempt[Case[A, B, Any]] = {
    def errOrCase(b: B, opt: Option[Case[A, B, Any]]) =
      Attempt.fromOption(opt, new UnknownDiscriminator(b))
    if (cases.forall(_.condition.isLeft)) {
      // we reverse the cases so earlier cases 'win' in event of overlap
      val tbl = cases.reverse.map(kase => kase.condition.left.toOption.get -> kase).toMap
      b => errOrCase(b, tbl.get(b))
    } else {
      // this could be a bit smarter, but we fall back to linear scan
      b =>
        errOrCase(b, cases.find(_.condition.fold(_ == b, _._2(b))))
    }
  }

  /**
    * Replaces the current framing logic with the specified codec transformation.
    *
    * Every representative codec is wrapped with the framing logic when encoding/decoding.
    *
    * @param framing new framing logic
    * @group discriminator
    */
  def framing(framing: CodecTransformation): DiscriminatorCodec[A, B] =
    new DiscriminatorCodec[A, B](by, cases, framing)

  def sizeBound =
    by.sizeBound + SizeBound.choice(cases.iterator.map { c =>
      framing(c.prism.repCodec).sizeBound
    })

  def encode(a: A) =
    cases.iterator
      .flatMap { k =>
        k.prism
          .preview(a)
          .map { r =>
            by.encode(k.representative)
              .flatMap { bits =>
                framing(k.prism.repCodec).encode(r).map(bits ++ _)
              }
          }
          .map(List(_))
          .getOrElse(List())
      }
      .toStream
      .headOption match {
      case None    => Attempt.failure(new Err.MatchingDiscriminatorNotFound(a))
      case Some(r) => r
    }

  def decode(bits: BitVector) =
    (for {
      b <- by
      k <- Decoder.liftAttempt(matcher(b))
      r <- framing(k.prism.repCodec)
    } yield k.prism.review(r)).decode(bits)

  override def toString = s"discriminated($by)"
}

/** Companion for [[Discriminator]]. */
private[codecs] object DiscriminatorCodec {

  /** Provides an injection between `A` and `R` and a `Codec[R]`. */
  private[codecs] final case class Prism[A, R](
      preview: A => Option[R],
      review: R => A,
      repCodec: Codec[R]
  )

  /**
    * Maps a discrimination tag to a prism that supports encoding/decoding a value of type `A`.
    *
    * The `condition` field identifies the tag value this case applies to. The condition can be
    * specified by a single value of type `B` or by a tuple `(B, B => Boolean)`. The `prism`
    * field provides an injection to a representative type.
    *
    * To understand this class, it is best to consider encoding and decoding separately.
    *
    * == Encoding ==
    * When encoding a value of type `A`, this case applies if the `prism` returns a `Some` from
    * `prism.preview(value)`. Upon receiving a `Some`, the `condition` is used to generate a
    * discrimination tag. There are two cases to consider -- when the `conditional` is a Left
    * and a right. In the case of a left, the left value is used as the discrimination tag.
    * In the case of a right, the first element of the right tuple is used as the discrimination tag.
    *
    * The discrimination tag is encoded followed by the result of encoding the value using `prism.repCodec`
    * to encode the `Some` value returned from `prism.preview(value)`.
    *
    * == Decoding ==
    * When decoding, the discriminator tag is decoded from the bit vector and then each case is consulted.
    * This case applies if the `condition` applies to the decoded discriminator tag. Hence, there are
    * two cases to consider -- when the `conditional` is a left and a right. In the case of a left,
    * this case applies when the decoded discriminated tag is equal to the left value. In the case of a right,
    * this case applies when the function in the second position of the right tuple returns true when applied
    * with the decoded discriminated tag.
    *
    * If this case applies, then the `prism.repCodec` is used used to decode a value of some type `R`, which is
    * then converted to a value of type `A` via `prism.review`.
    */
  private[codecs] final case class Case[A, B, R](
      condition: Either[B, (B, B => Boolean)], // either a literal `B`, or a `B` predicate
      prism: Prism[A, R]
  ) {
    // make sure that if condition is (x: X, f: X => Boolean), that
    // `f(x)` is true, otherwise this case will fail to match itself
    // on decoding!
    condition.right.toOption.foreach {
      case (representative, matches) =>
        if (matches(representative)) true
        else sys.error(s"representative failed predicate: $representative")
    }

    def representative: B = condition.fold(identity, _._1)
  }
}
