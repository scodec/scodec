package scodec
package codecs

import scalaz.{\/-, -\/}
import scalaz.std.option.{none, some}
import scalaz.syntax.id._


private[codecs] final class ConditionalCodec[A](included: Boolean, codec: Codec[A]) extends Codec[Option[A]] {

  override def encode(a: Option[A]) = {
    a.filter { _ => included }.fold(BitVector.empty.right[String]) { a => codec.encode(a) }
  }

  override def decode(buffer: BitVector) = {
    if (included)
      codec.decode(buffer).map { case (rest, result) => (rest, some(result)) }
    else
      \/-((buffer, none))
  }

  override def toString = s"conditional($included, $codec)"

}
