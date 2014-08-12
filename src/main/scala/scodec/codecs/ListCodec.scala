package scodec
package codecs

import scalaz.{\/, \/-, -\/, Semigroup}
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.std.option._

import scodec.bits.BitVector

private[codecs] final class ListCodec[A](codec: Codec[A]) extends Codec[List[A]] {

  def encode(as: List[A]) = {
    as.traverseU { v => codec.encode(v) }.map { _.concatenate }
  }

  def decode(buffer: BitVector): String \/ (BitVector, List[A]) = {
    val bldr = List.newBuilder[A]
    var remaining = buffer
    var error: Option[String] = None
    while (remaining.nonEmpty) {
      codec.decode(remaining) match {
        case \/-((rest, value)) =>
          bldr += value
          remaining = rest
        case -\/(err) =>
          error = Some(err)
          remaining = BitVector.empty
      }
    }
    error.toLeftDisjunction((BitVector.empty, bldr.result))
  }

  override def toString = s"list($codec)"

}
