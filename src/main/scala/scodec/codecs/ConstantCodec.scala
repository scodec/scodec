package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._


class ConstantCodec(constant: BitVector, validate: Boolean = true) extends Codec[Unit] {

  override def encode(ignore: Unit) =
    \/.right(constant)

  override def decode(buffer: BitVector) =
    if (validate)
      buffer.consume(constant.size) { b => if (b == constant) Right(()) else Left(buffer + " not " + constant) }.disjunction
    else
      \/.right((buffer drop constant.size, ()))

  override def toString = s"constant($constant)"

}
