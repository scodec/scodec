package scodec
package codecs

import scalaz.\/-
import scodec.bits.BitVector

private[codecs] object BitVectorCodec extends Codec[BitVector] {
  override def encode(buffer: BitVector) = \/-(buffer)
  override def decode(buffer: BitVector) = \/-((BitVector.empty, buffer))
}
