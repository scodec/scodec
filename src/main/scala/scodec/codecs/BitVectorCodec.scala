package scodec
package codecs

import scodec.bits.BitVector

private[codecs] object BitVectorCodec extends Codec[BitVector] {
  override def encode(buffer: BitVector) = Attempt.successful(buffer)
  override def decode(buffer: BitVector) = Attempt.successful(DecodeResult(buffer, BitVector.empty))
  override def toString = "bits"
}
