package scodec

import shapeless._
import shapeless.ops.hlist._

/** Operations on `HList`s that are not provided by Shapeless. */
object HListOps {

  /**
   * Computes the inverse of `(k: K).filterNot[Unit]` -- i.e., inserts unit values wherever the unit type
   * appears in `K`.
   * @tparam K type that may have `Unit` params
   * @tparam L equivalent to `K` with `Unit` params filtered out
   * @param l list to insert unit values in to
   * @return new list with unit values inserted
   */
  def reUnit[K <: HList, L <: HList](l: L)(implicit ru: ReUnit[K, L]): K = ru(l)

  /** Provides the `reUnit` method on an `HList`. */
  implicit class ReUnitSyntax[L <: HList](val l: L) extends AnyVal {
    def reUnit[K <: HList](implicit ru: ReUnit[K, L]): K = ru(l)
  }

  /** Typeclass that allows computation of the inverse of calling `filterNot[Unit]` on a `K`. */
  sealed trait ReUnit[K <: HList, L <: HList] {
    def apply(l: L): K
  }

  object ReUnit {
    implicit lazy val base: ReUnit[HNil, HNil] = new ReUnit[HNil, HNil] {
      def apply(l: HNil): HNil = HNil
    }

    implicit def `non-empty K and L where head of K and L are same type`[H, KT <: HList, LT <: HList](implicit
      reUnit: ReUnit[KT, LT],
      notUnit: H =:!= Unit
    ): ReUnit[H :: KT, H :: LT] = new ReUnit[H :: KT, H :: LT] {
      def apply(l: H :: LT): H :: KT =
         l.head :: reUnit(l.tail)
    }

    implicit def `non-empty K and any L where head of K is Unit`[KT <: HList, L <: HList](implicit
      reUnit: ReUnit[KT, L]
    ): ReUnit[Unit :: KT, L] = new ReUnit[Unit :: KT, L] {
      def apply(l: L): Unit :: KT =
         () :: reUnit(l)
    }
  }
}
