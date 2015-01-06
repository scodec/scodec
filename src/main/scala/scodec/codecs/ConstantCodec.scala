package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ConstantCodec(constant: BitVector, validate: Boolean = true) extends Codec[Unit] {

  override def encode(ignore: Unit) =
    EncodeResult.successful(constant)

  override def decode(buffer: BitVector) =
    if (validate) {
      buffer.acquire(constant.size) match {
        case Left(e) => DecodeResult.failure(Err.insufficientBits(constant.size, buffer.size))
        case Right(b) =>
          if (b == constant) DecodeResult.successful((), buffer.drop(constant.size)) 
          else DecodeResult.failure(Err(s"expected constant $constant but got $b"))
      }
    } else DecodeResult.successful((), buffer drop constant.size)

  override def toString = s"constant($constant)"
}

