package scodec
package codecs

import scalaz.\/-


private[codecs] object BitVectorCodec extends Codec[BitVector] {
  override def encode(buffer: BitVector) = \/-(buffer)
  override def decode(buffer: BitVector) = \/-((BitVector.empty, buffer))
}
