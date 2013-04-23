package scodec

import Codec.DecodingContext


class TupleCodec[A, B](codecA: Codec[A], codecB: Codec[B]) extends Codec[(A, B)] {

  override def encode(t: (A, B)) = for {
    a <- codecA.encode(t._1)
    b <- codecB.encode(t._2)
  } yield a ++ b

  override def decode(buffer: BitVector) = (for {
    a <- DecodingContext(codecA.decode)
    b <- DecodingContext(codecB.decode)
  } yield (a, b)).run(buffer)

}
