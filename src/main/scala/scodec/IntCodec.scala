package scodec

import scalaz.syntax.id._
import scalaz.syntax.std.option._

import java.nio.ByteBuffer


class IntCodec(bits: Int, signed: Boolean = true) extends Codec[Int] {

  require(bits > 0 && bits <= (if (signed) 32 else 31), "bits must be in range [1, 32] for signed and [1, 31] for unsigned")

  val MaxValue = (1 << (if (signed) (bits - 1) else bits)) - 1
  val MinValue = if (signed) -(1 << (bits - 1)) else 0

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} integer"

  override def encode(i: Int) = {
    if (i > MaxValue) {
      s"$i is greater than maximum value $MaxValue for $description".left
    } else if (i < MinValue) {
      s"$i is less than minimum value $MinValue for $description".left
    } else {
      val buffer = ByteBuffer.allocate(4).putInt(i)
      buffer.flip()
      (BitVector(buffer) << (32 - bits)).take(bits).right
    }
  }

  override def decode(buffer: BitVector) =
    buffer.consume(bits) { _.padTo(32).rightShift(32 - bits, signed).asByteBuffer.getInt.right }
}
