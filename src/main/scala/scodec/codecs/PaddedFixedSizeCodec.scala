package scodec
package codecs

import scodec.bits.BitVector
import scala.annotation.tailrec

private[codecs] final class PaddedFixedSizeCodec[A](size: Long, codec: Codec[A], padCodec: Long => Codec[Unit]) extends Codec[A] {
  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        Attempt.failure(Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits"))
      else if (encoded.size == size)
        Attempt.successful(encoded)
      else {
        val pc = padCodec(size - encoded.size)
        pc.encode(()) match {
          case Attempt.Successful(padding) =>
            @tailrec def pad(bits: BitVector): Attempt[BitVector] = {
              if (bits.size == size) Attempt.successful(bits)
              else if (bits.size < size) pad(bits ++ padding)
              else Attempt.failure(Err(s"padding overflows fixed size field of $size bits").pushContext("padding"))
            }
            pad(encoded)

          case Attempt.Failure(err) =>
            Attempt.failure(err.pushContext("padding"))
        }
      }
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match {
      case Left(e) => Attempt.failure(Err.insufficientBits(size, buffer.size))
      case Right(b) =>
        codec.decode(b) match {
          case e @ Attempt.Failure(_) => e
          case Attempt.Successful(DecodeResult(res, rest)) =>
            val pc = padCodec(rest.size)
            @tailrec def validate(bits: BitVector): Attempt[DecodeResult[A]] =
              if (bits.isEmpty)
                Attempt.successful(DecodeResult(res, buffer.drop(size)))
              else
                pc.decode(bits) match {
                  case Attempt.Failure(err) => Attempt.failure(err.pushContext("padding"))
                  case Attempt.Successful(DecodeResult(_, remain)) => validate(remain)
                }
            validate(rest)
        }
    }

  override def toString = s"paddedFixedSizeBits($size, $codec)"
}

