package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class IgnoreCodec(bits: Long) extends Codec[Unit] {

  override def encode(unit: Unit) =
    EncodeResult.successful(BitVector.low(bits))

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(e) => DecodeResult.failure(Err.insufficientBits(bits, buffer.size))
      case Right(_) => DecodeResult.successful((), buffer.drop(bits))
    }

  override def toString = s"ignore($bits bits)"
}
