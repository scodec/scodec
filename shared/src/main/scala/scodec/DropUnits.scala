package scodec

import scala.compiletime._

// Note: top-level recursive match types are not currently supported (https://github.com/lampepfl/dotty/issues/6362)
// When that is fixed, `DropUnits.T[A]` should be moved to a top-level type `DropUnits[A]`

object DropUnits {
  /** The tuple which is the result of removing all 'Unit' types from the tuple 'A'. */
  type T[A <: Tuple] <: Tuple = A match {
    case hd *: tl => hd match {
      case Unit => T[tl]
      case _ => hd *: T[tl]
    }
    case EmptyTuple => EmptyTuple
  }

  inline def drop[A <: Tuple](a: A): T[A] = {
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
  }.asInstanceOf[T[A]]

  inline def insert[A <: Tuple](t: T[A]): A = {
    inline erasedValue[A] match {
      case _: (Unit *: tl) => (()) *: (insert[tl](t.asInstanceOf[T[tl]]))
      case _: (hd *: tl) =>
        val t2 = t.asInstanceOf[NonEmptyTuple]
        t2.head.asInstanceOf[hd] *: insert[tl](t2.tail.asInstanceOf[T[tl]])
      case EmptyTuple => EmptyTuple
    }
  }.asInstanceOf[A]
}
