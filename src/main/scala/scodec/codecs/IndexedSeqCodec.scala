package scodec
package codecs

import scala.collection.immutable.IndexedSeq

import scalaz.{\/, \/-, -\/, Semigroup}
import scalaz.std.indexedSeq._
import scalaz.syntax.traverse._
import scalaz.syntax.std.option._


class IndexedSeqCodec[A](codec: Codec[A]) extends Codec[IndexedSeq[A]] {

  def encode(ixSeq: IndexedSeq[A]) = {
    ixSeq.traverseU { v => codec.encode(v) }.map { _.concatenate }
  }

  def decode(buffer: BitVector): String \/ (BitVector, IndexedSeq[A]) = {
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

  override def toString = s"ixseq($codec)"

}
