package scodec

import shapeless._
import shapeless.ops.coproduct.Align

import scodec.codecs.DropUnits

/** Typeclass that describes type constructors that support the `exmap` operation. */
abstract class Transform[F[_]] { self =>

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Attempt[B]` and `B => Attempt[A]`.
    */
  def exmap[A, B](fa: F[A], f: A => Attempt[B], g: B => Attempt[A]): F[B]

  /**
    * Transforms supplied `F[A]` to an `F[B]` using the isomorphism described by two functions,
    * `A => B` and `B => A`.
    */
  def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] =
    exmap[A, B](fa, a => Attempt.successful(f(a)), b => Attempt.successful(g(b)))

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Attempt[B]` and `B => A`.
    *
    * The supplied functions form an injection from `B` to `A`. Hence, converting a `F[A]` to a `F[B]` converts from
    * a larger to a smaller type. Hence, the name `narrow`.
    */
  def narrow[A, B](fa: F[A], f: A => Attempt[B], g: B => A): F[B] =
    exmap(fa, f, b => Attempt.successful(g(b)))

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => B` and `B => Attempt[A]`.
    *
    * The supplied functions form an injection from `A` to `B`. Hence, converting a `F[A]` to a `F[B]` converts from
    * a smaller to a larger type. Hence, the name `widen`.
    */
  def widen[A, B](fa: F[A], f: A => B, g: B => Attempt[A]): F[B] =
    exmap[A, B](fa, a => Attempt.successful(f(a)), g)

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => B` and `B => Option[A]`.
    *
    * Particularly useful when combined with case class apply/unapply. E.g., `widenOpt(fa, Foo.apply, Foo.unapply)`.
    */
  def widenOpt[A, B](fa: F[A], f: A => B, g: B => Option[A]): F[B] =
    exmap[A, B](
      fa,
      a => Attempt.successful(f(a)),
      b => Attempt.fromOption(g(b), Err(s"widening failed: $b"))
    )

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

/** Companion for [[Transformer]]. */
object Transformer {

  /** Identity transformer. */
  implicit def id[A]: Transformer[A, A] = new Transformer[A, A] {
    def apply[F[_]: Transform](fa: F[A]): F[A] = fa
  }

  /** Builds a `Transformer[A, B]` from a Shapeless `Generic.Aux[A, B]`. */
  implicit def fromGeneric[A, B](implicit gen: Generic.Aux[A, B]): Transformer[A, B] =
    new Transformer[A, B] {
      def apply[F[_]: Transform](fa: F[A]): F[B] = fa.xmap(a => gen.to(a), b => gen.from(b))
    }

  /** Builds a `Transformer[A, B]` from a Shapeless `Generic.Aux[B, A]`. */
  implicit def fromGenericReverse[A, B](implicit gen: Generic.Aux[B, A]): Transformer[A, B] =
    new Transformer[A, B] {
      def apply[F[_]: Transform](fa: F[A]): F[B] = fa.xmap(a => gen.from(a), b => gen.to(b))
    }

  /** Builds a `Transformer[A, B]` from a Shapeless `Generic` for `A` where the representation is an `HList` which is compatible with the `HList B` with units removed. */
  implicit def fromGenericWithUnitsHList[A, Repr <: HList, B <: HList](
      implicit gen: Generic.Aux[A, Repr],
      du: DropUnits.Aux[B, Repr]
  ): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] =
      fa.xmap(a => du.addUnits(gen.to(a)), b => gen.from(du.removeUnits(b)))
  }

  /** Builds a `Transformer[A, B]` from a Shapeless `Generic` for `B` where the representation is an `HList` which is compatible with the `HList A` with units removed. */
  implicit def fromGenericWithUnitsHListReverse[A <: HList, Repr <: HList, B](
      implicit gen: Generic.Aux[B, Repr],
      du: DropUnits.Aux[A, Repr]
  ): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] =
      fa.xmap(a => gen.from(du.removeUnits(a)), b => du.addUnits(gen.to(b)))
  }

  /** Builds a `Transformer[A, B]` for singleton case class `A` and value `B`. */
  implicit def fromGenericSingleton[A, B](
      implicit gen: Generic.Aux[A, B :: HNil]
  ): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] =
      fa.xmap(a => gen.to(a).head, b => gen.from(b :: HNil))
  }

  /** Builds a `Transformer[A, B]` for value `A` and singleton case class `B`. */
  implicit def fromGenericSingletonReverse[A, B](
      implicit gen: Generic.Aux[B, A :: HNil]
  ): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] =
      fa.xmap(a => gen.from(a :: HNil), b => gen.to(b).head)
  }

  /** Builds a `Transformer[A, B]` where `A` is a coproduct whose component types can be aligned with the coproduct representation of `B`. */
  implicit def fromGenericWithUnalignedCoproductReverse[B, Repr <: Coproduct, A <: Coproduct](
      implicit
      gen: Generic.Aux[B, Repr],
      toAligned: Align[Repr, A],
      fromAligned: Align[A, Repr]
  ): Transformer[A, B] = new Transformer[A, B] {
    def apply[F[_]: Transform](fa: F[A]): F[B] =
      fa.xmap(a => gen.from(fromAligned(a)), b => toAligned(gen.to(b)))
  }
}
