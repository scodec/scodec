package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class RecoverCodec(target: Codec[Unit], lookahead: Boolean) extends Codec[Boolean] {

  def encode(a: Boolean) = target.encode(())

  def decode(buffer: BitVector) =
    target.decode(buffer) match {
      case DecodeResult.Successful(_, rest) => DecodeResult.successful(true, if (lookahead) buffer else rest)
      case f: DecodeResult.Failure => DecodeResult.successful(false, buffer)
    }

  override def toString = if (lookahead) s"lookahead($target)" else s"recover($target)"
}
