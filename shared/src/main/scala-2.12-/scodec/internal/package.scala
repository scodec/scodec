package scodec

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

private[scodec] object compat {
  type Factory[-A, +C] = CanBuildFrom[Nothing, A, C]

  implicit class FactoryOps[-A, +C](private val factory: Factory[A, C]) {
    def newBuilder: Builder[A, C] = factory()
  }
}
