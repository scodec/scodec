package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class FixedSizeCodec[A](size: Long, codec: Codec[A]) extends Codec[A] {

  override def sizeBound = SizeBound.exact(size)

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        Attempt.failure(Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits"))
      else
        Attempt.successful(encoded.padTo(size))
    }
  } yield result

  override def decode(buffer: BitVector) = {
    if (buffer.sizeGreaterThanOrEqual(size)) {
      codec.decode(buffer.take(size)) map { res =>
        DecodeResult(res.value, buffer.drop(size))
      }
    } else {
      Attempt.failure(Err.insufficientBits(size, buffer.size))
    }
  }

  override def toString = s"fixedSizeBits($size, $codec)"
}
