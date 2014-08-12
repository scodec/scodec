package scodec
package codecs

import scalaz.{\/, \/-, -\/, Semigroup}
import scalaz.std.vector._
import scalaz.syntax.traverse._
import scalaz.syntax.std.option._

import scodec.bits.BitVector

private[codecs] final class VectorCodec[A](codec: Codec[A]) extends Codec[Vector[A]] {

  def encode(vector: Vector[A]) = {
    vector.traverseU { v => codec.encode(v) }.map { _.concatenate }
  }

  def decode(buffer: BitVector): String \/ (BitVector, Vector[A]) = {
    val bldr = Vector.newBuilder[A]
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

  override def toString = s"vector($codec)"

}
