package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class IdentityCodec extends Codec[BitVector] {
  override def sizeBound = SizeBound.unknown
  override def encode(bits: BitVector) = Attempt.successful(bits)
  override def decode(bits: BitVector) = Attempt.successful(DecodeResult(bits, BitVector.empty))
  override def toString = "identity"
}
