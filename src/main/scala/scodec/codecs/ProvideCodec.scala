package scodec
package codecs

import scalaz.syntax.id._

import scodec.bits.BitVector

/**
 * Codec that provides a constant value from decode and ignores the value to encode.
 *
 * Useful as a combinator with [[DiscriminatorCodec]].
 */
private[codecs] final class ProvideCodec[A](value: A) extends Codec[A] {
  override def encode(a: A) = BitVector.empty.right
  override def decode(bv: BitVector) = (bv, value).right
  override def toString = s"provide($value)"
}
