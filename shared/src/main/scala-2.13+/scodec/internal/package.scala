package scodec

private[scodec] object compatInternal {
  type Factory[-A, +C] = scala.collection.Factory[A, C]
}
