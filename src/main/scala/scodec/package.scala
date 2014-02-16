import scalaz.{ \/, Monoid, StateT }
import shapeless._
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
 * reconstitue a `Codec` from the `GenCodec` by calling `fuse`.
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
  type DecodingContext[+A] = StateT[({type λ[+a] = String \/ a})#λ, BitVector, A]

  implicit val bitVectorMonoidInstance: Monoid[BitVector] = Monoid.instance(_ ++ _, BitVector.empty)

  implicit val byteVectorMonoidInstance: Monoid[ByteVector] = Monoid.instance(_ ++ _, ByteVector.empty)

  /** Provides common operations on a `Codec[HList]`. */
  final implicit class HListCodecEnrichedWithHListSupport[L <: HList](val selfHList: Codec[L]) extends AnyVal {
    import codecs.HListCodec._

    /** Returns a new codec representing `Codec[A :: L]`. */
    def ::[A](a: Codec[A]): Codec[A :: L] = prepend(a, selfHList)

    /** Returns a new codec that encodes/decodes `A :: L` but only returns `L`.  HList equivalent of `~>`. */
    def :~>:[A: scalaz.Monoid](a: Codec[A]): Codec[L] = Codec.dropLeft(a, selfHList)

    /** Returns a new codec that encodes/decodes the `HList L` followed by an `A`. */
    def :+[A, LA <: HList](a: Codec[A])(implicit
      prepend: PrependAux[L, A :: HNil, LA],
      init: Init[LA] { type Out = L },
      last: Last[LA] { type Out = A }
    ): Codec[LA] =
      append(selfHList, a)

    /** Returns a new codec the encodes/decodes the `HList K` followed by the `HList L`. */
    def :::[K <: HList, KL <: HList, KLen <: Nat](k: Codec[K])(implicit
      prepend: PrependAux[K, L, KL],
      lengthK: Length[K] { type Out = KLen },
      split: Split[KL, KLen] { type P = K; type S = L }
    ): Codec[KL] = concat(k, selfHList)
  }

  /** Provides `HList` related syntax for codecs of any type. */
  final implicit class ValueCodecEnrichedWithHListSupport[A](val self: Codec[A]) extends AnyVal {
    import codecs.HListCodec._

    /** Creates a new codec that encodes/decodes an `HList` of `B :: A :: HNil`. */
    def ::[B](codecB: Codec[B]): Codec[B :: A :: HNil] =
      codecs.HListCodec.prepend(codecB, prepend(self, hnilCodec))

    def flatPrepend[L <: HList](f: A => Codec[L]): Codec[A :: L] = codecs.HListCodec.flatPrepend(self, f)

    def flatZipHList[B](f: A => Codec[B]): Codec[A :: B :: HNil] = flatPrepend(f andThen (_.hlist))

    /** Operator alias for `flatPrepend`. */
    def >>:~[L <: HList](f: A => Codec[L]): Codec[A :: L] = flatPrepend(f)
  }
}
