package scodec
package codecs

import scodec.bits.BitVector

private[codecs] object BooleanCodec extends Codec[Boolean] {

  override def encode(b: Boolean) =
    EncodeResult.successful(if (b) BitVector.high(1) else BitVector.low(1))

  override def decode(buffer: BitVector) =
    buffer.acquire(1) match {
      case Left(e) => DecodeResult.failure(Err.insufficientBits(1, 0))
      case Right(b) => DecodeResult.successful(b.head, buffer.tail)
    }

  override def toString = "bool"
}
