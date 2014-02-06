package scodec
package codecs

import scalaz.syntax.id._

/**
 * Codec that provides a constant value from decode and ignores the value to encode.
 *
 * Useful as a combinator with [[DiscriminatorCodec]].
 */
class ProvideCodec[A](value: A) extends Codec[A] {
  override def encode(a: A) = BitVector.empty.right
  override def decode(bv: BitVector) = (bv, value).right
  override def toString = s"provide($value)"
}
