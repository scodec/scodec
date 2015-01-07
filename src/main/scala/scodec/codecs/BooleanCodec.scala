package scodec
package codecs

import scodec.bits.BitVector

private[codecs] object BooleanCodec extends Codec[Boolean] {

  override def encode(b: Boolean) =
    Attempt.successful(if (b) BitVector.high(1) else BitVector.low(1))

  override def decode(buffer: BitVector) =
    buffer.acquire(1) match {
      case Left(e) => Attempt.failure(Err.insufficientBits(1, 0))
      case Right(b) => Attempt.successful(DecodeResult(b.head, buffer.tail))
    }

  override def toString = "bool"
}
