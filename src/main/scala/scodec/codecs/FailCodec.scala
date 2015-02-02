package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class FailCodec[A](encErr: Err, decErr: Err) extends Codec[A] {

  override def sizeBound = SizeBound.unknown

  override def encode(a: A) = Attempt.failure(encErr)

  override def decode(b: BitVector) = Attempt.failure(decErr)

  override def toString = "fail"
}
