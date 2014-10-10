package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._

import scodec.bits.BitVector

private[codecs] final class IgnoreCodec(bits: Long) extends Codec[Unit] {

  override def encode(unit: Unit) =
    \/.right(BitVector.low(bits))

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(e) => \/.left(Err.insufficientBits(bits, buffer.size))
      case Right(_) => \/.right((buffer.drop(bits), ()))
    }

  override def toString = s"ignore($bits bits)"
}
