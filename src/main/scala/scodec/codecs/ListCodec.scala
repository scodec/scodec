package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ListCodec[A](codec: Codec[A], limit: Option[Int] = None) extends Codec[List[A]] {

  def encode(list: List[A]) = Encoder.encodeSeq(codec)(list)

  def decode(buffer: BitVector) = Decoder.decodeCollect[List, A](codec, limit)(buffer)

  override def toString = s"list($codec)"
}
