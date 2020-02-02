package scodec
package codecs

/**
  * Describes an isomorphism between two tuples, `K` and `L`, where `L` has the same shape as `K` except unit
  * values have been removed.
  */
sealed trait DropUnits[K <: Tuple] {
  type L <: Tuple
  def removeUnits(k: K): L
  def addUnits(l: L): K
}

/** Low priority implicits for [[DropUnits]]. */
sealed trait DropUnitsLowPriority {

  /* Keep this low priority so that head of `K` is checked for Unit before this is used. This avoids computing
   * H =!:= Unit for each component type.
   */
  given `non-empty K and L where head of K and L are same type`[H, KT <: Tuple, LT <: Tuple](
      given rest: DropUnits.Aux[KT, LT]
  ): DropUnits.Aux[H *: KT, H *: LT] = new DropUnits[H *: KT] {
    type L = H *: LT
    def removeUnits(k: H *: KT): H *: LT = k.head *: rest.removeUnits(k.tail)
    def addUnits(l: H *: LT): H *: KT = l.head *: rest.addUnits(l.tail)
  }
}

/** Companion for [[DropUnits]]. */
object DropUnits extends DropUnitsLowPriority {

  type Aux[K0 <: Tuple, L0 <: Tuple] = DropUnits[K0] { type L = L0 }

  given base: DropUnits.Aux[Unit, Unit] = new DropUnits[Unit] {
    type L = Unit
    def removeUnits(k: Unit): Unit = ()
    def addUnits(l: Unit): Unit = ()
  }

  given `non-empty K and any L where head of K is Unit`[KT <: Tuple, L0 <: Tuple](
      given rest: DropUnits.Aux[KT, L0]
  ): DropUnits.Aux[Unit *: KT, L0] = new DropUnits[Unit *: KT] {
    type L = L0
    def removeUnits(k: Unit *: KT): L = rest.removeUnits(k.tail)
    def addUnits(l: L): Unit *: KT = (()) *: rest.addUnits(l)
  }
}
