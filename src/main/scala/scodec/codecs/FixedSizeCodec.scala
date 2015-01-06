package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class FixedSizeCodec[A](size: Long, codec: Codec[A]) extends Codec[A] {

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        EncodeResult.failure(Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits"))
      else
        EncodeResult.successful(encoded.padTo(size))
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match {
      case Left(e) => DecodeResult.failure(Err.insufficientBits(size, buffer.size))
      case Right(b) =>
        codec.decode(b) match {
          case e @ DecodeResult.Failure(_) => e
          case DecodeResult.Successful(res, rest) => DecodeResult.successful(res, buffer.drop(size))
        }
    }

  override def toString = s"fixedSizeBits($size, $codec)"
}
