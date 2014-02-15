package scodec
package codecs

import scalaz.\/
import \/.{left, right}
import DiscriminatorCodec.Case

object DiscriminatorCodecSyntax {

  sealed trait NeedDiscriminatorCodec[A] {
    def by[B](discriminatorCodec: Codec[B]): DiscriminatorBuilder[A, B]
  }

  /**
   * Provides syntax for building up a set of cases when creating
   * a [[DiscriminatorCodec]]. See [[DiscriminatorCodecSyntax.discriminated]]
   * for complete documentation.
   */
  class DiscriminatorBuilder[A,B](by: Codec[B], cases: Vector[Case[A,B]]) {

    def ?[R](tag: B)(f: A => Option[R])(inj: R => A)(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](by, cases :+ Case(left(tag), (f, inj, cr)))

    def ?[R](tag: B, g: B => Boolean)(f: A => Option[R])(inj: R => A)(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](by, cases :+ Case(right(tag -> g), (f, inj, cr)))

    def |[R](tag: B)(f: PartialFunction[A,R])(inj: R => A)(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](by, cases :+ Case(left(tag), (f.lift, inj, cr)))

    def |[R](tag: B, g: B => Boolean)(f: PartialFunction[A,R])(inj: R => A)(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](by, cases :+ Case(right(tag -> g), (f.lift, inj, cr)))

    def \[R <: A](tag: B)(f: PartialFunction[A,R])(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](
        by,
        cases :+ Case[A,B](left(tag), (f.lift, (r: R) => r, cr)))

    def \[R <: A](tag: B, g: B => Boolean)(f: PartialFunction[A,R])(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](
        by,
        cases :+ Case[A,B](right(tag -> g), (f.lift, (r: R) => r, cr)))

    def /[R <: A](tag: B)(f: A => Option[R])(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](
        by,
        cases :+ Case[A,B](left(tag), (f, (r: R) => r, cr)))

    def /[R <: A](tag: B, g: B => Boolean)(f: A => Option[R])(cr: Codec[R]) =
      new DiscriminatorBuilder[A,B](
        by,
        cases :+ Case[A,B](right(tag -> g), (f, (r: R) => r, cr)))

    def build: Codec[A] = new DiscriminatorCodec[A,B](by, cases)
  }

  /**
   * Provides syntax for building a [[DiscriminatorCodec]].
   * Usage: {{{
   val codecA: Codec[A] = ...
   val codecB: Codec[B] = ...

   val codecE: Codec[Either[A,B]] =
     discriminated[Either[A,B]].by(uint8)
     .| (0) { case Left(l) => l } (Left.apply) (codecA)
     .| (1) { case Right(r) => r } (Right.apply) (codecB)
     .build
   }}}

   This encodes an `Either[A,B]` by checking the given patterns
   in sequence from top to bottom. For the first pattern that matches,
   it emits the corresponding discriminator value: `0` for `Left`
   and `1` for `Right`, encoded via the `uint8` codec. It then emits
   either an encoded `A`, encoded using `codecA`, or an encoded `B`,
   using `codecB`.

   Decoding is the mirror of this; the returned `codecE` will first
   read an `Int`, using the `uint8` codec. If it is a `0`, it then
   runs `codecA`, and injects the result into `Either` via `Left.apply`.
   If it is a `1`, it runs `codecB` and injects the result into `Either`
   via `Right.apply`.

   There are a few variations on this syntax, depending on whether you
   have a `PartialFunction` from the base type or an `B => Option[S]`
   function from the base type to the subcase.

   If you you already have a codec specific to the case, you can omit
   the 'injection' function. For instance: {{{
     val leftCodec: Codec[Left[A,B]] = codecA.pxmap(Left.apply, Left.unapply)
     val rightCodec: Codec[Right[A,B]] = codecB.pxmap(Left.apply, Left.unapply)
     val codecE: Codec[Either[A,B]] =
       discriminated[Either[A,B]].by(uint8)
       .\ (0) { case l@Left(_) => l } (leftCodec) // backslash instead of '|'
       .\ (1) { case r@Right(_) => r } (rightCodec)
   }}}

   The actual formatted bits are identical with either formulation.
   */
  final def discriminated[A]: NeedDiscriminatorCodec[A] = new NeedDiscriminatorCodec[A] {
    final def by[B](discriminatorCodec: Codec[B]): DiscriminatorBuilder[A, B] =
      new DiscriminatorBuilder[A, B](discriminatorCodec, Vector())
  }
}
