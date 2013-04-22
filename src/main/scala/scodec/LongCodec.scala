package scodec

import scalaz.syntax.id._
import scalaz.syntax.std.option._

import java.nio.ByteBuffer


class LongCodec(bits: Int, signed: Boolean = true) extends Codec[Long] {

  require(bits > 0 && bits <= (if (signed) 64 else 63), "bits must be in range [1, 64] for signed and [1, 63] for unsigned")

  val MaxValue = (1L << (if (signed) (bits - 1) else bits)) - 1
  val MinValue = if (signed) -(1L << (bits - 1)) else 0

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} integer"

  override def encode(i: Long) = {
    if (i > MaxValue) {
      s"$i is greater than maximum value $MaxValue for $description".left
    } else if (i < MinValue) {
      s"$i is less than minimum value $MinValue for $description".left
    } else {
      val buffer = ByteBuffer.allocate(8).putLong(i)
      buffer.flip()
      (BitVector(buffer) << (64 - bits)).take(bits).right
    }
  }

  override def decode(buffer: BitVector) =
    buffer.consume(bits) { _.padTo(64).rightShift(64 - bits, signed).asByteBuffer.getLong.right }

  override def toString = description + " codec"
}
