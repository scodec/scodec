import language.higherKinds

import scalaz.{ \/, Monoid, StateT }
import shapeless._
import ops.hlist.{Prepend, RightFolder, Init, Last, Length, Split, FilterNot, Mapper}
import poly._
import scodec.bits._

/**
 * Combinator library for working with binary data.
 *
 * The primary abstraction of this library is [[Codec]], which provides the ability to encode/decode values to/from binary.
 *
 * There are more general abstractions though, such as [[Encoder]] and [[Decoder]]. There's also [[GenCodec]] which extends
 * both `Encoder` and `Decoder` but allows the types to vary. Given these more general abstractions, a `Codec[A]` can be
 * represented as a `GenCodec[A, A]`.
 *
 * The more general abstractions are important because they allow operations on codecs that would not otherwise be possible.
 * For example, given a `Codec[A]`, mapping a function `A => B` over the codec yields a `GenCodec[A, B]`. Without the
 * more general abstractions, `map` is impossible to define (e.g., how would `codec.map(f).encode(b)` be implemented?).
 * Given a `GenCodec[A, B]`, the encoding functionality can be ignored by treating it as a `Decoder[B]`, or the encoding
 * type can be changed via `contramap`. If after further transformations, the two types to `GenCodec` are equal, we can
 * reconstitute a `Codec` from the `GenCodec` by calling `fuse`.
 *
 * See the [[codecs]] package object for pre-defined codecs for many common data types and combinators for building larger
 * codecs out of smaller ones.
 *
 * For the categorically minded, note the following:
 *  - `Decoder` is a monad
 *  - `Encoder` is a contravariant functor
 *  - `GenCodec` is a profunctor
 *  - `Codec` is an invariant functor
 *
 * Each type has the corresponding Scalaz typeclass defined in its companion object.
 */
package object scodec {

  /** Alias for state/either transformer that simplifies calling decode on a series of codecs, wiring the remaining bit vector of each in to the next entry. */
  type DecodingContext[A] = StateT[({type λ[a] = Err \/ a})#λ, BitVector, A]

  implicit val bitVectorMonoidInstance: Monoid[BitVector] = Monoid.instance(_ ++ _, BitVector.empty)

  implicit val byteVectorMonoidInstance: Monoid[ByteVector] = Monoid.instance(_ ++ _, ByteVector.empty)

  /**
   * Provides method syntax for working with a type constructor that has a [[Transform]] typeclass instance.
   */
  implicit class TransformSyntax[F[_], A](val fa: F[A])(implicit t: Transform[F]) {

    /**
     * Transforms using two functions, `A => Err \/ B` and `B => Err \/ A`.
     * @group combinators
     */
    def exmap[B](f: A => Err \/ B, g: B => Err \/ A): F[B] = t.exmap(fa, f, g)

    /**
     * Transforms using the isomorphism described by two functions, `A => B` and `B => A`.
     * @group combinators
     */
    def xmap[B](f: A => B, g: B => A): F[B] = t.xmap(fa, f, g)

    /**
     * Transforms using two functions, `A => Err \/ B` and `B => A`.
     *
     * The supplied functions form an injection from `B` to `A`. Hence, this method converts from
     * a larger to a smaller type. Hence, the name `narrow`.
     * @group combinators
     */
    def narrow[B](f: A => Err \/ B, g: B => A): F[B] = t.narrow(fa, f, g)

    /**
     * Transforms using two functions, `A => B` and `B => Err \/ A`.
     *
     * The supplied functions form an injection from `A` to `B`. Hence, this method converts from
     * a smaller to a larger type. Hence, the name `widen`.
     * @group combinators
     */
    def widen[B](f: A => B, g: B => Err \/ A): F[B] = t.widen(fa, f, g)

    /**
     * Transforms using two functions, `A => B` and `B => Option[A]`.
     *
     * Particularly useful when combined with case class apply/unapply. E.g., `pxmap(fa, Foo.apply, Foo.unappy)`.
     */
    def pxmap[B](f: A => B, g: B => Option[A]): F[B] = t.pxmap(fa, f, g)

    /**
     * Transforms using implicitly available evidence that such a transformation is possible.
     *
     * Typical transformations include converting:
     *  - an `F[L]` for some `L <: HList` to/from an `F[CC]` for some case class `CC`, where the types in the case class are
     *    aligned with the types in `L`
     *  - an `F[C]` for some `C <: Coproduct` to/from an `F[SC]` for some sealed class `SC`, where the component types in
     *    the coproduct are the leaf subtypes of the sealed class.
     * @group combinators
     */
    def as[B](implicit as: Transformer[A, B]): F[B] = as(fa)
  }

  /** Provides common operations on a `Codec[HList]`. */
  final implicit class HListCodecEnrichedWithHListSupport[L <: HList](val self: Codec[L]) extends AnyVal {
    import codecs.HListCodec

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec representing `Codec[B :: L]`.
     * That is, this operator is a codec-level `HList` prepend operation.
     * @param codec codec to prepend
     * @group hlist
     */
    def ::[B](codec: Codec[B]): Codec[B :: L] = HListCodec.prepend(codec, self)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that encodes/decodes
     * `B :: L` but only returns `L`.  HList equivalent of `~>`.
     * @group hlist
     */
    def :~>:[B](codec: Codec[B])(implicit ev: Unit =:= B): Codec[L] = codec.dropLeft(self)

    /**
     * Creates a new codec with all unit values filtered out.
     * @group hlist
     */
    def dropUnits[M <: HList](implicit fltr: FilterNot.Aux[L, Unit, M], ru: HListOps.ReUnit[M, L]): Codec[M] =
      HListCodec.dropUnits[L, M](self)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that encodes/decodes
     * the `HList L` followed by a `B`.
     * That is, this operator is a codec-level `HList` append operation.
     * @group hlist
     */
    def :+[B, LB <: HList](codec: Codec[B])(implicit
      prepend: Prepend.Aux[L, B :: HNil, LB],
      init: Init.Aux[LB, L],
      last: Last.Aux[LB, B]
    ): Codec[LB] = HListCodec.append(self, codec)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec the encodes/decodes
     * the `HList K` followed by the `HList L`.
     * @group hlist
     */
    def :::[K <: HList, KL <: HList, KLen <: Nat](k: Codec[K])(implicit
      prepend: Prepend.Aux[K, L, KL],
      lengthK: Length.Aux[K, KLen],
      split: Split.Aux[KL, KLen, (K, L)]
    ): Codec[KL] = HListCodec.concat(k, self)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec the encodes/decodes
     * the `HList L` followed by the `HList M`, where the latter is encoded/decoded with the codec
     * returned from applying `L` to `f`.
     * @group hlist
     */
    def flatConcat[M <: HList, LM <: HList, LLen <: Nat](f: L => Codec[M])(implicit
      prepend: Prepend.Aux[L, M, LM],
      lengthK: Length.Aux[L, LLen],
      split: Split.Aux[LM, LLen, (L, M)]
    ): Codec[LM] = HListCodec.flatConcat(self, f)

    /**
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec the encodes/decodes
     * the `HList L` followed by the value `A`, where the latter is encoded/decoded with the codec
     * returned from applying `L` to `f`.
     * @group hlist
     */
    def flatAppend[A, LA <: HList, Len <: Nat](f: L => Codec[A])(implicit
      prepend: Prepend.Aux[L, A :: HNil, LA],
      length: Length.Aux[L, Len],
      split: Split.Aux[LA, Len, (L, A :: HNil)]
    ): Codec[LA] = HListCodec.flatAppend(self, f)

    /**
     * Polymorphic function version of `xmap`.
     *
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that's the result of
     * xmapping with `p` and `q`, using `p` to convert from `L` to `M` and using `q` to convert from
     * `M` to `L`.
     *
     * @param p polymorphic function that converts from `L` to `M`
     * @param q polymorphic function that converts from `M` to `L`
     * @group generic
     */
    def polyxmap[M <: HList](p: Poly, q: Poly)(implicit lToM: Mapper.Aux[p.type, L, M], mToL: Mapper.Aux[q.type, M, L]): Codec[M] =
      self.xmap(_ map p, _ map q)

    /**
     * Polymorphic function version of `xmap` that uses a single polymorphic function in both directions.
     *
     * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that's the result of
     * xmapping with `p` for both forward and reverse directions.
     *
     * @param p polymorphic function that converts from `L` to `M` and from `M` to `L`
     * @group generic
     */
    def polyxmap1[M <: HList](p: Poly)(implicit m: Mapper.Aux[p.type, L, M], m2: Mapper.Aux[p.type, M, L]): Codec[M] =
      polyxmap(p, p)
  }

  /** Provides `HList` related syntax for codecs of any type. */
  final implicit class ValueCodecEnrichedWithHListSupport[A](val self: Codec[A]) extends AnyVal {
    import codecs.HListCodec

    /**
     * When called on a `Codec[A]` where `A` is not a subytpe of `HList`, creates a new codec that encodes/decodes an `HList` of `B :: A :: HNil`.
     * For example, {{{uint8 :: utf8}}} has type `Codec[Int :: String :: HNil]`.
     * @group hlist
     */
    def ::[B](codecB: Codec[B]): Codec[B :: A :: HNil] =
      codecB :: self :: HListCodec.hnilCodec

    /**
     * When called on a `Codec[A]`, returns a new codec that encodes/decodes `B :: A :: HNil`.
     * HList equivalent of `~>`.
     * @group hlist
     */
    def :~>:[B](codecB: Codec[B])(implicit ev: Unit =:= B): Codec[A :: HNil] =
      codecB :~>: self.hlist

    /**
     * Creates a new codec that encodes/decodes an `HList` type of `A :: L` given a function `A => Codec[L]`.
     * This allows later parts of an `HList` codec to be dependent on earlier values.
     * @group hlist
     */
    def flatPrepend[L <: HList](f: A => Codec[L]): Codec[A :: L] = HListCodec.flatPrepend(self, f)

    /**
     * Creates a new codec that encodes/decodes an `HList` type of `A :: L` given a function `A => Codec[L]`.
     * This allows later parts of an `HList` codec to be dependent on earlier values.
     * Operator alias for `flatPrepend`.
     * @group hlist
     */
    def >>:~[L <: HList](f: A => Codec[L]): Codec[A :: L] = flatPrepend(f)

    /**
     * Creates a new codec that encodes/decodes an `HList` type of `A :: B :: HNil` given a function `A => Codec[B]`.
     * If `B` is an `HList` type, consider using `flatPrepend` instead, which avoids nested `HLists`.
     * This is the direct `HList` equivalent of `flatZip`.
     * @group hlist
     */
    def flatZipHList[B](f: A => Codec[B]): Codec[A :: B :: HNil] = flatPrepend(f andThen (_.hlist))
  }

  /** Provides syntax related to generic programming for codecs of any type. */
  final implicit class ValueCodecEnrichedWithGenericSupport[A](val self: Codec[A]) extends AnyVal {

    /**
     * Polymorphic function version of `xmap`.
     *
     * When called on a `Codec[A]` where `A` is not a subytpe of `HList`, returns a new codec that's the result of
     * xmapping with `p` and `q`, using `p` to convert from `A` to `B` and using `q` to convert from
     * `B` to `A`.
     *
     * @param p polymorphic function that converts from `A` to `B`
     * @param q polymorphic function that converts from `B` to `A`
     * @group generic
     */
    def polyxmap[B](p: Poly, q: Poly)(implicit aToB: Case.Aux[p.type, A :: HNil, B], bToA: Case.Aux[q.type, B :: HNil, A]): Codec[B] =
      self.xmap(aToB, bToA)

    /**
     * Polymorphic function version of `xmap` that uses a single polymorphic function in both directions.
     *
     * When called on a `Codec[A]` where `A` is not a subytpe of `HList`, returns a new codec that's the result of
     * xmapping with `p` for both forward and reverse directions.
     *
     * @param p polymorphic function that converts from `A` to `B` and from `B` to `A`
     * @group generic
     */
    def polyxmap1[B](p: Poly)(implicit aToB: Case.Aux[p.type, A :: HNil, B], bToA: Case.Aux[p.type, B :: HNil, A]): Codec[B] =
      polyxmap(p, p)
  }

  /** Provides additional methods on `HList`s. */
  final implicit class EnrichedHList[L <: HList](val self: L) extends AnyVal {
    /**
     * Converts an `HList` of codecs in to a single codec.
     * That is, converts `Codec[X0] :: Codec[X1] :: ... :: Codec[Xn] :: HNil` in to a `Codec[X0 :: X1 :: ... :: Xn :: HNil].
     */
    def toCodec(implicit to: codecs.ToHListCodec[L]): to.Out = to(self)
  }

  /** Provides methods specific to encoders of Shapeless coproducts. */
  final implicit class EnrichedCoproductEncoder[C <: Coproduct](val self: Encoder[C]) extends AnyVal {
    /**
     * When called on a `Encoder[C]` where `C` is a coproduct containing type `A`, converts to an `Encoder[A]`.
     * @group coproduct
     */
    def selectEncoder[A](implicit inj: ops.coproduct.Inject[C, A]): Encoder[A] = self.contramap { a => Coproduct[C](a) }
  }

  /** Provides methods specific to decoders of Shapeless coproducts. */
  final implicit class EnrichedCoproductDecoder[C <: Coproduct](val self: Decoder[C]) extends AnyVal {
    /**
     * When called on a `Decoder[C]` where `C` is a coproduct containing type `A`, converts to a `Decoder[Option[A]]`.
     * @group coproduct
     */
    def selectDecoder[A](implicit sel: ops.coproduct.Selector[C, A]): Decoder[Option[A]] = self.map { c => c.select[A] }
  }
}
