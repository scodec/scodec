package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ListCodec[A](codec: Codec[A], limit: Option[Int] = None)
    extends Codec[List[A]] {

  def sizeBound = limit match {
    case None      => SizeBound.unknown
    case Some(lim) => codec.sizeBound * lim.toLong
  }

  def encode(list: List[A]) = codec.encodeAll(list)

  def decode(buffer: BitVector) = codec.collect(buffer, limit)

  override def toString = s"list($codec)"
}
