package scodec
package codecs

import scalaz.{\/, \/-, -\/}
import scodec.bits.BitVector

private[codecs] final class RecoverCodec(target: Codec[Unit], lookahead: Boolean) extends Codec[Boolean] {

  def encode(a: Boolean): String \/ BitVector =
    target.encode(())

  def decode(buffer: BitVector): String \/ (BitVector, Boolean) = {
    target.decode(buffer) match {
      case \/-((rest, _)) => \/-((if (lookahead) buffer else rest, true))
      case -\/(_) => \/-((buffer, false))
    }
  }

  override def toString = if (lookahead) s"lookahead($target)" else s"recover($target)"
}
