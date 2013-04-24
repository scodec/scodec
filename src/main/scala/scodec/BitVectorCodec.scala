package scodec

import scalaz.\/-


object BitVectorCodec extends Codec[BitVector] {
  override def encode(buffer: BitVector) = \/-(buffer)
  override def decode(buffer: BitVector) = \/-((BitVector.empty, buffer))
}
