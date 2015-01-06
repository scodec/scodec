package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class IdentityCodec extends Codec[BitVector] {
  override def encode(bits: BitVector) = EncodeResult.successful(bits)
  override def decode(bits: BitVector) = DecodeResult.successful(bits, BitVector.empty)
  override def toString = "identity"
}
