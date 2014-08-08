package scodec
package codecs

import scalaz.\/

import scodec.bits.BitVector

private[codecs] final class FailCodec[A](encMessage: String, decMessage: String) extends Codec[A] {

  override def encode(a: A) = \/.left(encMessage)

  override def decode(b: BitVector) = \/.left(decMessage)

  override def toString = "fail"
}
