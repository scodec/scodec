package scodec
package codecs

import shapeless._

/**
  * Describes an isomorphism between two `HList`s, `K` and `L`, where `L` has the same shape as `K` except unit
  * values have been removed.
  */
sealed trait DropUnits[K <: HList] {
  type L <: HList
  def removeUnits(k: K): L
  def addUnits(l: L): K
}

/** Low priority implicits for [[DropUnits]]. */
sealed trait DropUnitsLowPriority {

  /* Keep this low priority so that head of `K` is checked for Unit before this is used. This avoids computing
   * H =!:= Unit for each component type.
   */
  implicit def `non-empty K and L where head of K and L are same type`[H, KT <: HList, LT <: HList](
      implicit
      rest: DropUnits.Aux[KT, LT]
  ): DropUnits.Aux[H :: KT, H :: LT] = new DropUnits[H :: KT] {
    type L = H :: LT
    def removeUnits(k: H :: KT): H :: LT = k.head :: rest.removeUnits(k.tail)
    def addUnits(l: H :: LT): H :: KT = l.head :: rest.addUnits(l.tail)
  }
}

/** Companion for [[DropUnits]]. */
object DropUnits extends DropUnitsLowPriority {

  type Aux[K0 <: HList, L0 <: HList] = DropUnits[K0] { type L = L0 }

  implicit lazy val base: DropUnits.Aux[HNil, HNil] = new DropUnits[HNil] {
    type L = HNil
    def removeUnits(k: HNil): HNil = HNil
    def addUnits(l: HNil): HNil = HNil
  }

  implicit def `non-empty K and any L where head of K is Unit`[KT <: HList, L0 <: HList](
      implicit
      rest: DropUnits.Aux[KT, L0]
  ): DropUnits.Aux[Unit :: KT, L0] = new DropUnits[Unit :: KT] {
    type L = L0
    def removeUnits(k: Unit :: KT): L = rest.removeUnits(k.tail)
    def addUnits(l: L): Unit :: KT = () :: rest.addUnits(l)
  }
}
