package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class RecoverCodec(target: Codec[Unit], lookahead: Boolean)
    extends Codec[Boolean] {

  def sizeBound = target.sizeBound

  def encode(a: Boolean) = if (a) target.encode(()) else Attempt.successful(BitVector.empty)

  def decode(buffer: BitVector) =
    target.decode(buffer) match {
      case Attempt.Successful(DecodeResult(_, rest)) =>
        Attempt.successful(DecodeResult(true, if (lookahead) buffer else rest))
      case _: Attempt.Failure => Attempt.successful(DecodeResult(false, buffer))
    }

  override def toString = if (lookahead) s"lookahead($target)" else s"recover($target)"
}
