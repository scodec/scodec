package scodec

import shapeless._
import shapeless.ops.hlist._

/** Operations on `HList`s that are not provided by Shapeless. */
object HListOps {

  /**
   * Computes the inverse of `(k: K).filterNot[L]` -- i.e., inserts unit values wherever the unit type
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
    implicit def fltr: FilterNot.Aux[K, Unit, L]
    def apply(l: L): K
  }

  object ReUnit {
    implicit lazy val base: ReUnit[HNil, HNil] = new ReUnit[HNil, HNil] {
      implicit lazy val fltr: FilterNot.Aux[HNil, Unit, HNil] = implicitly[FilterNot.Aux[HNil, Unit, HNil]]
      def apply(l: HNil): HNil = HNil
    }

    implicit def `non-empty K and empty L`[KH, KT <: HList](implicit
      reUnit: ReUnit[KT, HNil],
      fltrK: FilterNot.Aux[KH :: KT, Unit, HNil],
      headEv: Unit =:= KH
    ): ReUnit[KH :: KT, HNil] = new ReUnit[KH :: KT, HNil] {
      implicit def fltr = fltrK
      def apply(l: HNil): KH :: KT =
        () :: reUnit(l)
    }

    implicit def `non-empty K and L where head of K and L are same type`[KH, KT <: HList, LH, LT <: HList](implicit
      reUnit: ReUnit[KT, LT],
      fltrAll: FilterNot.Aux[KH :: KT, Unit, LH :: LT],
      headEv: LH =:= KH
    ): ReUnit[KH :: KT, LH :: LT] = new ReUnit[KH :: KT, LH :: LT] {
      implicit def fltr: FilterNot.Aux[KH :: KT, Unit, LH :: LT] = fltrAll
      def apply(l: LH :: LT): KH :: KT =
         headEv(l.head) :: reUnit(l.tail)
    }

    implicit def `non-empty K and L where head of K is Unit`[KH, KT <: HList, LH, LT <: HList](implicit
      reUnit: ReUnit[KT, LH :: LT],
      fltrAll: FilterNot.Aux[KH :: KT, Unit, LH :: LT],
      headEv: Unit =:= KH
    ): ReUnit[KH :: KT, LH :: LT] = new ReUnit[KH :: KT, LH :: LT] {
      implicit def fltr: FilterNot.Aux[KH :: KT, Unit, LH :: LT] = fltrAll
      def apply(l: LH :: LT): KH :: KT =
         headEv(()) :: reUnit(l)
    }
  }
}
