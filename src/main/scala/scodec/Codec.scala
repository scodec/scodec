package scodec

import scalaz.\/

trait Codec[A] {
  def encode(a: A): Error \/ BitVector
  def decode(bits: BitVector): Error \/ (BitVector, A)
}
