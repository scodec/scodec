package scodec

import scalaz.Monoid


class TupleCodec[A, B](codecA: Codec[A], codecB: Codec[B]) extends Codec[(A, B)] {

  override def encode(t: (A, B)) =
    Codec.encodeBoth(codecA, codecB)(t._1, t._2)

  override def decode(buffer: BitVector) =
    Codec.decodeBoth(codecA, codecB)(buffer)

  override def toString = s"($codecA, $codecB)"
}

