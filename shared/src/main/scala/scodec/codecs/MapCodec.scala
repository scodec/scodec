package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class MapCodec[A, B](keyCodec: Codec[A], valueCodec: Codec[B], limit: Option[Int] = None) extends Codec[Map[A, B]] {

  val itemCodec: Codec[(A, B)] = keyCodec ~ valueCodec

  val listCodec: Codec[List[(A, B)]] = new ListCodec(itemCodec, limit)

  def sizeBound = limit match {
    case None => SizeBound.unknown
    case Some(lim) => itemCodec.sizeBound * lim.toLong
  }

  def encode(map: Map[A, B]) = listCodec.encode(map.toList)

  def decode(buffer: BitVector) = listCodec.decode(buffer).map(_.map(_.toMap))

  override def toString = s"map($keyCodec, $valueCodec)"

}