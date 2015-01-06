package scodec
package codecs

import scodec.bits.BitVector
import scala.annotation.tailrec

private[codecs] final class PaddedFixedSizeCodec[A](size: Long, codec: Codec[A], padCodec: Long => Codec[Unit]) extends Codec[A] {
  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        EncodeResult.failure(Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits"))
      else if (encoded.size == size)
        EncodeResult.successful(encoded)
      else {
        val pc = padCodec(size - encoded.size)
        pc.encode(()) match {
          case EncodeResult.Successful(padding) =>
            @tailrec def pad(bits: BitVector): EncodeResult = {
              if (bits.size == size) EncodeResult.successful(bits)
              else if (bits.size < size) pad(bits ++ padding)
              else EncodeResult.failure(Err(s"padding overflows fixed size field of $size bits").pushContext("padding"))
            }
            pad(encoded)

          case EncodeResult.Failure(err) =>
            EncodeResult.failure(err.pushContext("padding"))
        }
      }
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match {
      case Left(e) => DecodeResult.failure(Err.insufficientBits(size, buffer.size))
      case Right(b) =>
        codec.decode(b) match {
          case e @ DecodeResult.Failure(_) => e
          case DecodeResult.Successful(res, rest) =>
            val pc = padCodec(rest.size)
            @tailrec def validate(bits: BitVector): DecodeResult[A] =
              if (bits.isEmpty)
                DecodeResult.successful(res, buffer.drop(size))
              else
                pc.decode(bits) match {
                  case DecodeResult.Failure(err) => DecodeResult.failure(err.pushContext("padding"))
                  case DecodeResult.Successful(_, remain) => validate(remain)
                }
            validate(rest)
        }
    }

  override def toString = s"paddedFixedSizeBits($size, $codec)"
}

