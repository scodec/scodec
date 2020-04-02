package scodec

import scala.deriving.Mirror

/** Typeclass that describes type constructors that support the `exmap` operation. */
trait Transform[F[_]] {

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Attempt[B]` and `B => Attempt[A]`.
    */
  def [A, B](fa: F[A]).exmap(f: A => Attempt[B], g: B => Attempt[A]): F[B]

  /**
    * Transforms supplied `F[A]` to an `F[B]` using the isomorphism described by two functions,
    * `A => B` and `B => A`.
    */
  def [A, B](fa: F[A]).xmap(f: A => B, g: B => A): F[B] =
    fa.exmap(a => Attempt.successful(f(a)), b => Attempt.successful(g(b)))

  /** Curried version of [[xmap]]. */
  inline def [A, B](fa: F[A]).xmapc(f: A => B)(g: B => A): F[B] = fa.xmap(f, g)

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Attempt[B]` and `B => A`.
    *
    * The supplied functions form an injection from `B` to `A`. Hence, converting a `F[A]` to a `F[B]` converts from
    * a larger to a smaller type. Hence, the name `narrow`.
    */
  def [A, B](fa: F[A]).narrow(f: A => Attempt[B], g: B => A): F[B] =
    fa.exmap(f, b => Attempt.successful(g(b)))

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => B` and `B => Attempt[A]`.
    *
    * The supplied functions form an injection from `A` to `B`. Hence, converting a `F[A]` to a `F[B]` converts from
    * a smaller to a larger type. Hence, the name `widen`.
    */
  def [A, B](fa: F[A]).widen(f: A => B, g: B => Attempt[A]): F[B] =
    fa.exmap(a => Attempt.successful(f(a)), g)

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => B` and `B => Option[A]`.
    *
    * Particularly useful when combined with case class apply/unapply. E.g., `widenOpt(fa, Foo.apply, Foo.unapply)`.
    */
  def [A, B](fa: F[A]).widenOpt(f: A => B, g: B => Option[A]): F[B] =
    fa.exmap(
      a => Attempt.successful(f(a)),
      b => Attempt.fromOption(g(b), Err(s"widening failed: $b"))
    )
}
