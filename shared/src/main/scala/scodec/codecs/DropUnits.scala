package scodec
package codecs

import scala.compiletime._

object DropUnits {
  type T[A <: Tuple] <: Tuple = A match {
    case hd *: tl => hd match {
      case Unit => T[tl]
      case _ => hd *: T[tl]
    }
    case Unit => Unit
  }

  inline def drop[A <: Tuple](a: A): T[A] = {
    inline erasedValue[A] match {
      case _: (Unit *: tl) => drop[tl](a.asInstanceOf[Unit *: tl].tail)
      case _: (hd *: tl) => a.asInstanceOf[hd *: tl].head *: drop[tl](a.asInstanceOf[hd *: tl].tail)
      case _: Unit => ()
    }
  }.asInstanceOf[T[A]]

  inline def insert[A <: Tuple](t: T[A]): A = {
    inline erasedValue[A] match {
      case _: (Unit *: tl) => (()) *: (insert[tl](t.asInstanceOf[T[tl]]))
      case _: (hd *: tl) =>
        val t2 = t.asInstanceOf[NonEmptyTuple]
        t2.head.asInstanceOf[hd] *: insert[tl](t2.tail.asInstanceOf[T[tl]])
      case _: Unit => ()
    }
  }.asInstanceOf[A]
}
