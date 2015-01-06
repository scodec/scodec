package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class FixedSizeCodec[A](size: Long, codec: Codec[A]) extends Codec[A] {

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        Attempt.failure(Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits"))
      else
        Attempt.successful(encoded.padTo(size))
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match {
      case Left(e) => Attempt.failure(Err.insufficientBits(size, buffer.size))
      case Right(b) =>
        codec.decode(b) match {
          case e @ Attempt.Failure(_) => e
          case Attempt.Successful(DecodeResult(res, rest)) => Attempt.successful(DecodeResult(res, buffer.drop(size)))
        }
    }

  override def toString = s"fixedSizeBits($size, $codec)"
}
