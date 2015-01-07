package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class VectorCodec[A](codec: Codec[A], limit: Option[Int] = None) extends Codec[Vector[A]] {

  def encode(vector: Vector[A]) = Encoder.encodeSeq(codec)(vector)

  def decode(buffer: BitVector) =
    Decoder.decodeCollect[Vector, A](codec, limit)(buffer)

  override def toString = s"vector($codec)"

}
