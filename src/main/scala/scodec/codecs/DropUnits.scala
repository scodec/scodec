package scodec
package codecs

import shapeless._
import shapeless.ops.hlist._

/**
 * Describes an isomorphism between two `HList`s, `K` and `L`, where `L` has the same shape as `K` except unit
 * values have been removed.
 */
sealed trait DropUnits[K <: HList, L <: HList] {
  def removeUnits(k: K): L
  def addUnits(l: L): K
}

/** Low priority implicits for [[DropUnits]]. */
sealed trait DropUnitsLowPriority {

  /* Keep this low priority so that head of `K` is checked for Unit before this is used. This avoids computing
   * H =!:= Unit for each component type.
   */
  implicit def `non-empty K and L where head of K and L are same type`[H, KT <: HList, LT <: HList](implicit
    rest: DropUnits[KT, LT]
  ): DropUnits[H :: KT, H :: LT] = new DropUnits[H :: KT, H :: LT] {
    def removeUnits(k: H :: KT): H :: LT = k.head :: rest.removeUnits(k.tail)
    def addUnits(l: H :: LT): H :: KT = l.head :: rest.addUnits(l.tail)
  }
}

/** Companion for [[DropUnits]]. */
object DropUnits extends DropUnitsLowPriority {

  implicit lazy val base: DropUnits[HNil, HNil] = new DropUnits[HNil, HNil] {
    def removeUnits(k: HNil): HNil = HNil
    def addUnits(l: HNil): HNil = HNil
  }

  implicit def `non-empty K and any L where head of K is Unit`[KT <: HList, L <: HList](implicit
    rest: DropUnits[KT, L]
  ): DropUnits[Unit :: KT, L] = new DropUnits[Unit :: KT, L] {
    def removeUnits(k: Unit :: KT): L = rest.removeUnits(k.tail)
    def addUnits(l: L): Unit :: KT = () :: rest.addUnits(l)
  }
}
