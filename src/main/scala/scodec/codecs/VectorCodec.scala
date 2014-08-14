package scodec
package codecs

import scalaz.\/

import scodec.bits.BitVector

private[codecs] final class VectorCodec[A](codec: Codec[A]) extends Codec[Vector[A]] {

  def encode(vector: Vector[A]): String \/ BitVector = Encoder.encodeSeq(codec)(vector)

  def decode(buffer: BitVector): String \/ (BitVector, Vector[A]) =
    Decoder.decodeCollect[Vector, A](codec)(buffer).map { res => (BitVector.empty, res) }

  override def toString = s"vector($codec)"

}
