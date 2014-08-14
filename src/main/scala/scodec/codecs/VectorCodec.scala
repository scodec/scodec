package scodec
package codecs

import scalaz.{\/, \/-, -\/}
import scalaz.syntax.std.option._

import scodec.bits.BitVector

private[codecs] final class VectorCodec[A](codec: Codec[A]) extends Codec[Vector[A]] {

  def encode(vector: Vector[A]): String \/ BitVector = {
    var acc = BitVector.empty
    val buf = new collection.mutable.ArrayBuffer[BitVector](vector.size)
    vector foreach { a =>
      codec.encode(a) match {
        case \/-(aa) => buf += aa
        case e @ -\/(_) => return e
      }
    }
    def merge(offset: Int, size: Int): BitVector = size match {
      case 0 => BitVector.empty
      case 1 => buf(offset)
      case n =>
        val half = size / 2
        merge(offset, half) ++ merge(offset + half, half + (if (size % 2 == 0) 0 else 1))
    }
    \/.right(merge(0, buf.size))
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
