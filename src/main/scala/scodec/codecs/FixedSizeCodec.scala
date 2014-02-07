package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._

private[codecs] final class FixedSizeCodec[A](size: Int, codec: Codec[A]) extends Codec[A] {

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        \/.left(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits")
      else
        \/.right(encoded.padTo(size))
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.consume(size) { b => codec.decode(b).map { case (rest, res) => res }.toEither }.disjunction

  override def toString = s"fixedSizeBits($size, $codec)"
}
