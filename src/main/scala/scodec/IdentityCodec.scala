package scodec

import scalaz.\/-


class IdentityCodec extends Codec[BitVector] {

  override def encode(bits: BitVector) =
    \/-(bits)

  override def decode(bits: BitVector) =
    \/-((BitVector.empty, bits))
}
