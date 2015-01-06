package scodec
package codecs

import scodec.bits.BitVector

private[codecs] object BitVectorCodec extends Codec[BitVector] {
  override def encode(buffer: BitVector) = EncodeResult.successful(buffer)
  override def decode(buffer: BitVector) = DecodeResult.successful(buffer, BitVector.empty)
  override def toString = "bits"
}
