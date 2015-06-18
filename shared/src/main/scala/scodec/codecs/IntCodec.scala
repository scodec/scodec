package scodec
package codecs

import java.nio.{ ByteBuffer, ByteOrder }

import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

private[codecs] final class IntCodec(bits: Int, signed: Boolean, ordering: ByteOrdering) extends Codec[Int] {

  require(bits > 0 && bits <= (if (signed) 32 else 31), "bits must be in range [1, 32] for signed and [1, 31] for unsigned")

  val MaxValue = (1 << (if (signed) (bits - 1) else bits)) - 1
  val MinValue = if (signed) -(1 << (bits - 1)) else 0

  private val bitsL = bits.toLong

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} integer"

  override def sizeBound = SizeBound.exact(bitsL)

  override def encode(i: Int) = {
    if (i > MaxValue) {
      Attempt.failure(Err(s"$i is greater than maximum value $MaxValue for $description"))
    } else if (i < MinValue) {
      Attempt.failure(Err(s"$i is less than minimum value $MinValue for $description"))
    } else {
      Attempt.successful(BitVector.fromInt(i, bits, ordering))
    }
  }

  override def decode(buffer: BitVector) = {
    if (buffer.sizeGreaterThanOrEqual(bitsL))
      Attempt.successful(DecodeResult(buffer.take(bitsL).toInt(signed, ordering), buffer.drop(bitsL)))
    else
      Attempt.failure(Err.insufficientBits(bitsL, buffer.size))
  }

  override def toString = description
}
