package scodec

import shapeless.{::, HList, HNil}

/** Provides source level compatibility aliases that mimic Scala 3 tuples with Shapeless HLists. */
object compat {
  val EmptyTuple = HNil
  type EmptyTuple = HNil
  type *:[+H, +T <: HList] = ::[H, T]
  val *: = ::
  implicit class TupleOps[T <: HList](private val t: T) extends AnyVal {
    def *:[H](h: H): H *: T = h :: t
  }
}
