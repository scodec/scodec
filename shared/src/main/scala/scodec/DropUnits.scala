package scodec

import scala.compiletime._

/** The tuple which is the result of removing all 'Unit' types from the tuple 'A'. */
type DropUnits[A <: Tuple] <: Tuple = A match {
  case hd *: tl => hd match {
    case Unit => DropUnits[tl]
    case _ => hd *: DropUnits[tl]
  }
  case EmptyTuple => EmptyTuple
}

object DropUnits {

  inline def drop[A <: Tuple](a: A): DropUnits[A] = {
    // Ideally, the following would work:
    // inline a match {
    //   case (_ *: t): (Unit *: tl) => drop[tl](t)
    //   case (h *: t): (hd *: tl) => h *: drop[tl](t)
    //   case EmptyTuple => EmptyTuple
    // }
    inline erasedValue[A] match {
      case _: (Unit *: tl) => drop[tl](a.asInstanceOf[Unit *: tl].tail)
      case _: (hd *: tl) => 
        val at = a.asInstanceOf[hd *: tl]
        at.head *: drop[tl](at.tail)
      case EmptyTuple => EmptyTuple
    }
  }.asInstanceOf[DropUnits[A]]

  inline def insert[A <: Tuple](t: DropUnits[A]): A = {
    inline erasedValue[A] match {
      case _: (Unit *: tl) => (()) *: (insert[tl](t.asInstanceOf[DropUnits[tl]]))
      case _: (hd *: tl) =>
        val t2 = t.asInstanceOf[NonEmptyTuple]
        t2.head.asInstanceOf[hd] *: insert[tl](t2.tail.asInstanceOf[DropUnits[tl]])
      case EmptyTuple => EmptyTuple
    }
  }.asInstanceOf[A]
}
