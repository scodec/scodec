package scodec
package codecs

import java.nio.{ ByteBuffer, ByteOrder }

import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

private[codecs] final class ShortCodec(bits: Int, signed: Boolean, ordering: ByteOrdering) extends Codec[Short] {

  require(bits > 0 && bits <= (if (signed) 16 else 15), "bits must be in range [1, 16] for signed and [1, 15] for unsigned")

  val MaxValue = ((1 << (if (signed) (bits - 1) else bits)) - 1).toShort
  val MinValue = (if (signed) -(1 << (bits - 1)) else 0).toShort

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} short"

  override def sizeBound = SizeBound.exact(bits.toLong)

  override def encode(s: Short) = {
    if (s > MaxValue) {
      Attempt.failure(Err(s"$s is greater than maximum value $MaxValue for $description"))
    } else if (s < MinValue) {
      Attempt.failure(Err(s"$s is less than minimum value $MinValue for $description"))
    } else {
      Attempt.successful(BitVector.fromShort(s, bits, ordering))
    }
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(bits.toLong) match {
      case Left(e) => Attempt.failure(Err.insufficientBits(bits.toLong, buffer.size))
      case Right(b) => Attempt.successful(DecodeResult(b.toShort(signed, ordering), buffer.drop(bits.toLong)))
    }

  override def toString = description
}
