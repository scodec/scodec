package scodec
package codecs

import scodec.bits.BitVector

/**
 * Codec that provides a constant value from decode and ignores the value to encode.
 *
 * Useful as a combinator with [[DiscriminatorCodec]].
 */
private[codecs] final class ProvideCodec[A](value: A) extends Codec[A] {
  override def encode(a: A) = Attempt.successful(BitVector.empty)
  override def decode(bv: BitVector) = Attempt.successful(DecodeResult(value, bv))
  override def toString = s"provide($value)"
}
