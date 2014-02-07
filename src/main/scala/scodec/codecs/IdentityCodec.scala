package scodec
package codecs

import scalaz.\/-

private[codecs] final class IdentityCodec extends Codec[BitVector] {

  override def encode(bits: BitVector) = \/-(bits)

  override def decode(bits: BitVector) = \/-((BitVector.empty, bits))
}
