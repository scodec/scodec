package scodec

private[scodec] object compat {
  type Factory[-A, +C] = scala.collection.Factory[A, C]
}
