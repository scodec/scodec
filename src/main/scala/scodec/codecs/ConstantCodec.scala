package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._

import scodec.bits.BitVector

private[codecs] final class ConstantCodec(constant: BitVector, validate: Boolean = true) extends Codec[Unit] {

  override def encode(ignore: Unit) =
    \/.right(constant)

  override def decode(buffer: BitVector) =
    if (validate) {
      buffer.acquire(constant.size) match {
        case Left(e) => \/.left(e)
        case Right(b) =>
          if (b == constant) \/.right((buffer.drop(constant.size), ())) else \/.left(s"expected constant $constant but got $b")
      }
    } else \/.right((buffer drop constant.size, ()))

  override def toString = s"constant($constant)"
}

