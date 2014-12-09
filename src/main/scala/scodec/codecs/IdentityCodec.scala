package scodec
package codecs

import scalaz.\/-

import scodec.bits.BitVector

private[codecs] final class IdentityCodec extends Codec[BitVector] {
  override def encode(bits: BitVector) = \/-(bits)
  override def decode(bits: BitVector) = \/-((BitVector.empty, bits))
  override def toString = "identity"
}
