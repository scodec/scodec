package scodec
package codecs

import scalaz.{ \/, \/-, -\/}
import scalaz.syntax.std.either._

import scalaz._
import Scalaz._ // TODO

import scodec.bits.BitVector
import scala.annotation.tailrec

private[codecs] final class PaddedFixedSizeCodec[A](size: Long, codec: Codec[A], padCodec: Codec[Unit]) extends Codec[A] {
  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        \/.left(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits")
      else {
        @tailrec def pad(bits: BitVector): String \/ BitVector = bits.size ?|?  size match {
          case Ordering.EQ => \/.right(bits)
          case Ordering.LT  => pad(bits ++ padCodec.encodeValid(()))
          case Ordering.GT => \/.left(s"[$padCodec] overflows")
        }
        pad(encoded)
      }
    }
  } yield result

   override def decode(buffer: BitVector) = //TODO
    buffer.acquire(size) match {
      case Left(e) => \/.left(e)
      case Right(b) =>
        codec.decode(b) match {
          case e @ -\/(_) => e
          case \/-((rest, res)) => \/-((buffer.drop(size), res))
        }
    }

  override def toString = s"paddedFixedSizeBits($size, $codec, $padCodec)"
}
