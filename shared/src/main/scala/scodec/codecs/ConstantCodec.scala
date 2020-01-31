package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ConstantCodec(constant: BitVector, validate: Boolean = true)
    extends Codec[Unit] {

  override def sizeBound = SizeBound.exact(constant.size)

  override def encode(ignore: Unit) =
    Attempt.successful(constant)

  override def decode(buffer: BitVector) =
    if (validate) {
      buffer.acquire(constant.size) match {
        case Left(e) => Attempt.failure(Err.insufficientBits(constant.size, buffer.size))
        case Right(b) =>
          if (b == constant) Attempt.successful(DecodeResult((), buffer.drop(constant.size)))
          else Attempt.failure(Err(s"expected constant $constant but got $b"))
      }
    } else Attempt.successful(DecodeResult((), buffer.drop(constant.size)))

  override def toString = s"constant($constant)"
}
