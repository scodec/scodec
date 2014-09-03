package scodec

import shapeless._
import shapeless.ops.hlist._

/** Operations on `HList`s that are not provided by Shapeless. */
object HListOps {

  /**
   * Computes the inverse of `(l: L).filterNot[Unit]` -- i.e., inserts unit values wherever the unit type
   * appears in `L`.
   * @tparam K HList type containing no `Unit` types
   * @tparam L equivalent to `K` with `Unit` types added in arbitrary positions
   * @param k list to insert unit values in to
   * @return new list with unit values inserted
   */
  def reUnit[K <: HList, L <: HList](k: K)(implicit ru: ReUnit[K, L]): L = ru(k)

  /** Provides the `reUnit` method on an `HList`. */
  implicit class ReUnitSyntax[K <: HList](val k: K) extends AnyVal {
    def reUnit[L <: HList](implicit ru: ReUnit[K, L]): L = ru(k)
  }

  /** Typeclass that allows computation of the inverse of calling `filterNot[Unit]` on a `L`. */
  sealed trait ReUnit[K <: HList, L <: HList] {
    def apply(l: K): L
  }

  object ReUnit {
    implicit lazy val base: ReUnit[HNil, HNil] = new ReUnit[HNil, HNil] {
      def apply(l: HNil): HNil = HNil
    }

    implicit def `non-empty K and L where head of K and L are same type`[H, KT <: HList, LT <: HList](implicit
      reUnit: ReUnit[KT, LT],
      notUnit: H =:!= Unit
    ): ReUnit[H :: KT, H :: LT] = new ReUnit[H :: KT, H :: LT] {
      def apply(k: H :: KT): H :: LT =
         k.head :: reUnit(k.tail)
    }

    implicit def `non-empty K and any L where head of L is Unit`[K <: HList, LT <: HList](implicit
      reUnit: ReUnit[K, LT]
    ): ReUnit[K, Unit :: LT] = new ReUnit[K, Unit :: LT] {
      def apply(k: K): Unit :: LT =
         () :: reUnit(k)
    }
  }
}
