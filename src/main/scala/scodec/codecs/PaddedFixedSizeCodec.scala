package scodec
package codecs

import scalaz.{ \/, \/-, -\/ }

import scalaz.syntax.order._
import scalaz.Ordering
import scalaz.std.AllInstances._

import scodec.bits.BitVector
import scala.annotation.tailrec

private[codecs] final class PaddedFixedSizeCodec[A](size: Long, codec: Codec[A], padCodec: Long => Codec[Unit]) extends Codec[A] {
  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        \/.left(Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits"))
      else if (encoded.size == size)
        \/.right(encoded)
      else {
        val pc = padCodec(size - encoded.size)
        pc.encode(()).fold(
          err => \/.left(err.pushContext("padding")),
          padding => {
            @tailrec def pad(bits: BitVector): Err \/ BitVector = bits.size ?|? size match {
              case Ordering.EQ => \/.right(bits)
              case Ordering.LT => pad(bits ++ padding)
              case Ordering.GT => \/.left(Err(s"padding overflows fixed size field of $size bits").pushContext("padding"))
            }
            pad(encoded)
          })
      }
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match {
      case Left(e) => \/.left(Err.insufficientBits(size, buffer.size))
      case Right(b) =>
        codec.decode(b) match {
          case e @ -\/(_) => e
          case \/-((rest, res)) => {
            val pc = padCodec(rest.size)
            @tailrec def validate(bits: BitVector): Err \/ (BitVector, A) =
              if (bits.isEmpty)
                \/-((buffer.drop(size), res))
              else
                pc.decode(bits) match {
                  case -\/(err) => \/.left(err.pushContext("padding"))
                  case \/-((remain, _)) => validate(remain)
                }
            validate(rest)
          }
        }
    }

  override def toString = s"paddedFixedSizeBits($size, $codec)"
}
