package scodec

import scalaz.{\/-, -\/}


class ConstantCodec(constant: BitVector, validate: Boolean = true) extends Codec[Unit] {

  override def encode(ignore: Unit) =
    \/-(constant)

  override def decode(buffer: BitVector) =
    if (validate)
      buffer.consume(constant.size) { b => if (b == constant) \/-(()) else -\/(buffer + " not " + constant)}
    else
      \/-((buffer drop constant.size, ()))

  override def toString = s"constant($constant)"

}
