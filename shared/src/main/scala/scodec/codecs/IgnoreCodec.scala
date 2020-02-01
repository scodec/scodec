package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class IgnoreCodec(bits: Long) extends Codec[Unit] {

  override def sizeBound = SizeBound.exact(bits)

  override def encode(unit: Unit) =
    Attempt.successful(BitVector.low(bits))

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(_)  => Attempt.failure(Err.insufficientBits(bits, buffer.size))
      case Right(_) => Attempt.successful(DecodeResult((), buffer.drop(bits)))
    }

  override def toString = s"ignore($bits bits)"
}
