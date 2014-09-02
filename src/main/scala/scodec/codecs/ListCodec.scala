package scodec
package codecs

import scalaz.\/

import scodec.bits.BitVector

private[codecs] final class ListCodec[A](codec: Codec[A], limit: Option[Int] = None) extends Codec[List[A]] {

  def encode(list: List[A]): String \/ BitVector = Encoder.encodeSeq(codec)(list)

  def decode(buffer: BitVector): String \/ (BitVector, List[A]) =
    Decoder.decodeCollect[List, A](codec, limit)(buffer)

  override def toString = s"list($codec)"
}
