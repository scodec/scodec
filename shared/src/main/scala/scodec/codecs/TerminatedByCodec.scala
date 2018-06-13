package scodec
package codecs

import scodec.bits.{BitVector, ByteVector}

final class TerminatedByCodec[A](codec: Codec[A], etx: ByteVector, retry: Err => Boolean) extends Codec[A] {

  // fails with insufficient bits with "best guess" for required bits
  private[this] def loop(bits: BitVector, from: Long): Attempt[DecodeResult[A]] =
    if (bits.size < from)
      Attempt.failure(Err.insufficientBits(bits.size + etx.bits.size, bits.size))
    else
      bits.bytes.indexOfSlice(etx, from) match {
        case -1 =>
          Attempt.failure(Err.insufficientBits(bits.size + etx.bits.size, bits.size))
        case i =>
          bits.bytes.splitAt(i) match {
            case (b1, b2) =>
              codec.decode(b1.bits)
                .map(_.mapRemainder(_ ++ b2.drop(etx.size).bits))
                .recoverWith {
                  case e if retry(e) => loop(bits, i + 1)
                }
          }
      }

  def decode(bits: BitVector): Attempt[DecodeResult[A]] = loop(bits, 0)

  def encode(value: A): Attempt[BitVector] = codec.encode(value).map(_ ++ etx.bits)

  def sizeBound: SizeBound = codec.sizeBound + SizeBound.exact(etx.bits.size)

  override def toString: String = s"$codec\\${etx.toHex}"
}
