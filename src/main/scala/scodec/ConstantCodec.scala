package scodec

import scalaz.{\/-, -\/}


class ConstantCodec(constant: BitVector) extends Codec[Unit] {

  override def encode(ignore: Unit) =
    \/-(constant)

  override def decode(buffer: BitVector) =
    \/-((buffer drop constant.size, ()))

}
