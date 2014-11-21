package scodec

import language.higherKinds

import scalaz.\/
import \/.right
import scalaz.syntax.std.option._

import shapeless._

/** Typeclass that describes type constructors that support the `exmap` operation. */
abstract class Transform[F[_]] { self =>

  /**
   * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Err \/ B` and `B => Err \/ A`.
   */
  def exmap[A, B](fa: F[A], f: A => Err \/ B, g: B => Err \/ A): F[B]

  /**
   * Transforms supplied `F[A]` to an `F[B]` using the isomorphism described by two functions,
   * `A => B` and `B => A`.
   */
  def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] =
    exmap(fa, right compose f, right compose g)

  /**
   * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Err \/ B` and `B => A`.
   *
   * The supplied functions form an injection from `B` to `A`. Hence, converting a `F[A]` to a `F[B]` converts from
   * a larger to a smaller type. Hence, the name `narrow`.
   */
  def narrow[A, B](fa: F[A], f: A => Err \/ B, g: B => A): F[B] =
    exmap(fa, f, right compose g)

  /**
   * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => B` and `B => Err \/ A`.
   *
   * The supplied functions form an injection from `A` to `B`. Hence, converting a `F[A]` to a `F[B]` converts from
   * a smaller to a larger type. Hence, the name `widen`.
   */
  def widen[A, B](fa: F[A], f: A => B, g: B => Err \/ A): F[B] =
    exmap(fa, right compose f, g)

  /**
   * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => B` and `B => Option[A]`.
   *
   * Particularly useful when combined with case class apply/unapply. E.g., `pxmap(fa, Foo.apply, Foo.unapply)`.
   */
  def pxmap[A, B](fa: F[A], f: A => B, g: B => Option[A]): F[B] =
    exmap(fa, right compose f, b => g(b) \/> Err(s"extraction failure: $b"))

  /**
   * Transforms supplied `F[A]` to an `F[B]` using implicitly available evidence that such a transformation
   * is possible.
   *
   * Typical transformations include converting:
   *  - an `F[L]` for some `L <: HList` to/from an `F[CC]` for some case class `CC`, where the types in the case class are
   *    aligned with the types in `L`
   *  - an `F[C]` for some `C <: Coproduct` to/from an `F[SC]` for some sealed class `SC`, where the component types in
   *    the coproduct are the leaf subtypes of the sealed class.
   */
  def as[A, B](fa: F[A])(implicit as: Transformer[A, B]): F[B] = as(fa)(self)
}

/** Companion for [[Transform]]. */
object Transform {
  def apply[F[_]](implicit t: Transform[F]): Transform[F] = t
}

/**
 * Witness operation that supports transforming an `F[A]` to an `F[B]` for all `F` which have a `Transform`
 * instance available.
 */
@annotation.implicitNotFound("""Could not prove that ${A} can be converted to/from ${B}.""")
abstract class Transformer[A, B] {
  def apply[F[_]: Transform](fa: F[A]): F[B]
}

/** Low priority `Transformer` builders. */
sealed abstract class TransformerLowPriority {

  /** Builds a `Transformer[A, B]` where `A` is a coproduct whose component types can be aligned with the coproduct representation of `B`. */
  implicit def alignCoproduct[B, Repr <: Coproduct, AlignedRepr <: Coproduct, A](implicit
    gen: Generic.Aux[B, Repr],
    aToAligned: A =:= AlignedRepr,
    alignedToA: AlignedRepr =:= A,
    toAligned: CoproductOps.Align[Repr, AlignedRepr],
    fromAligned: CoproductOps.Align[AlignedRepr, Repr]
  ): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] =
      fa.xmap(a => gen.from(fromAligned(aToAligned(a))), b => alignedToA(toAligned(gen.to(b))))
  }
}

/** Companion for [[Transformer]]. */
object Transformer extends TransformerLowPriority {

  /** Identity transformer. */
  implicit def id[A]: Transformer[A, A] = new Transformer[A, A] {
    def apply[F[_]: Transform](fa: F[A]): F[A] = fa
  }

  /** Builds a `Transformer[A, B]` from a Shapeless `Generic`. */
  implicit def fromGeneric[A, Repr, B](implicit gen: Generic.Aux[A, Repr], bToR: B =:= Repr, rToB: Repr =:= B): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] = fa.xmap(a => gen.to(a), b => gen.from(b))
  }

  /** Builds a `Transformer[A, B]` from a Shapeless `Generic`. */
  implicit def fromGenericReverse[A, Repr, B](implicit gen: Generic.Aux[B, Repr], aToR: A =:= Repr, rToA: Repr =:= A): Transformer[A, B]  = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] = fa.xmap(a => gen.from(a), b => gen.to(b))
  }

  /** Builds a `Transformer[A, B]` for value `A` and singleton case class `B`. */
  implicit def forSingletonReverse[A, Repr, B](implicit gen: Generic.Aux[B, Repr], aToR: (A :: HNil) =:= Repr, rToA: Repr =:= (A :: HNil)): Transformer[A, B]  = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] = fa.xmap(a => gen.from(a :: HNil), b => gen.to(b).head)
  }

  /** Builds a `Transformer[A, B]` for singleton case class `A` and value `B`. */
  implicit def mkAsSingletonReverse[A, Repr, B](implicit gen: Generic.Aux[A, Repr], bToR: (B :: HNil) =:= Repr, rToB: Repr =:= (B :: HNil)): Transformer[A, B]  = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] = fa.xmap(a => gen.to(a).head, b => gen.from(b :: HNil))
  }
}
