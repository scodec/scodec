package scodec
package codecs

import scalaz.\/

import scodec.bits.BitVector

private[codecs] final class VectorCodec[A](codec: Codec[A], limit: Option[Int] = None) extends Codec[Vector[A]] {

  def encode(vector: Vector[A]): Err \/ BitVector = Encoder.encodeSeq(codec)(vector)

  def decode(buffer: BitVector): Err \/ (BitVector, Vector[A]) =
    Decoder.decodeCollect[Vector, A](codec, limit)(buffer)

  override def toString = s"vector($codec)"

}
