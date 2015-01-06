package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class FailCodec[A](encErr: Err, decErr: Err) extends Codec[A] {

  override def encode(a: A) = EncodeResult.failure(encErr)

  override def decode(b: BitVector) = DecodeResult.failure(decErr)

  override def toString = "fail"
}
