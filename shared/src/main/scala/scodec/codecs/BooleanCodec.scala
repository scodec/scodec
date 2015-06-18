package scodec
package codecs

import scodec.bits.BitVector

private[codecs] object BooleanCodec extends Codec[Boolean] {

  override def sizeBound = SizeBound.exact(1)

  override def encode(b: Boolean) =
    Attempt.successful(if (b) BitVector.high(1) else BitVector.low(1))

  override def decode(buffer: BitVector) =
    if (buffer.isEmpty) Attempt.failure(Err.insufficientBits(1, 0))
    else Attempt.successful(DecodeResult(buffer.head, buffer.tail))

  override def toString = "bool"
}
