package scodec
package codecs

import scalaz.{ \/, \/-, -\/ }

import scalaz.syntax.order._
import scalaz.Ordering
import scalaz.std.AllInstances._

import scodec.bits.BitVector
import scala.annotation.tailrec

private[codecs] final class PaddedFixedSizeCodec[A](size: Long, codec: Codec[A], padCodec: Codec[Unit]) extends Codec[A] {
  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        \/.left(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits")
      else if (encoded.size == size)
        \/.right(encoded)
      else {
        padCodec.encode(()).fold(
          err => \/.left(s"Padding codec [$padCodec] failed : $err"),
          padding => {
            @tailrec def pad(bits: BitVector): String \/ BitVector = bits.size ?|? size match {
              case Ordering.EQ => \/.right(bits)
              case Ordering.LT => pad(bits ++ padding)
              case Ordering.GT => \/.left(s"[$a] padded by [$padCodec] overflows fixed size field of $size bits")
            }
            pad(encoded)
          })
      }
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match {
      case Left(e) => \/.left(e)
      case Right(b) =>
        codec.decode(b) match {
          case e @ -\/(_) => e
          case \/-((rest, res)) => {
            @tailrec def validate(bits: BitVector): String \/ (BitVector, A) =
              if (bits.isEmpty)
                \/-((buffer.drop(size), res))
              else
                padCodec.decode(bits) match {
                  case e @ -\/(_) => e
                  case \/-((remain, _)) => validate(remain)
                }
            validate(rest)
          }
        }
    }

  override def toString = s"paddedFixedSizeBits($size, $codec, $padCodec)"
}
