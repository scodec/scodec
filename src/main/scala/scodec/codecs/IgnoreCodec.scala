package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._

private[codecs] final class IgnoreCodec(bits: Int) extends Codec[Unit] {

  override def encode(unit: Unit) =
    \/.right(BitVector.low(bits))

  override def decode(buffer: BitVector) =
    buffer.consume(bits) { _ => Right(()) }.disjunction

  override def toString = s"ignore($bits bits)"
}
