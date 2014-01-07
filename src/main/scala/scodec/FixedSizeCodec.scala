package scodec

import scalaz.{\/-, -\/}


class FixedSizeCodec[A](size: Int, codec: Codec[A]) extends Codec[A] {

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    result <- {
      if (encoded.size > size)
        -\/(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits")
      else
        \/-(encoded.padTo(size))
    }
  } yield result

  override def decode(buffer: BitVector) =
    buffer.consume(size) { b => codec.decode(b) map { case (rest, res) => res } }

  override def toString = s"fixed-size($size, $codec)"
}
