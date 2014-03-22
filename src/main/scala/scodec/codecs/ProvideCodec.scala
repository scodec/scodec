package scodec
package codecs

import scalaz.\/

import scodec.bits.BitVector

/**
 * Codec that provides a constant value from decode and ignores the value to encode.
 *
 * Useful as a combinator with [[DiscriminatorCodec]].
 */
private[codecs] final class ProvideCodec[A](value: A) extends Codec[A] {
  override def encode(a: A) = \/.right(BitVector.empty)
  override def decode(bv: BitVector) = \/.right((bv, value))
  override def toString = s"provide($value)"
}
