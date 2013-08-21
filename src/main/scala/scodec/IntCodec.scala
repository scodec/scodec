package scodec

import scalaz.syntax.id._
import scalaz.syntax.std.option._

import java.nio.{ByteBuffer, ByteOrder}


class IntCodec(bits: Int, signed: Boolean = true, bigEndian: Boolean = true) extends Codec[Int] {

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
      val buffer = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(i)
      buffer.flip()
      val relevantBits = (BitVector(buffer) << (32 - bits)).take(bits)
      (if (bigEndian) relevantBits else relevantBits.reverseByteOrder).right
    }
  }

  override def decode(buffer: BitVector) =
    buffer.consume(bits) { b =>
      val mod = bits % 8
      var result = 0
      if (bigEndian) {
        b.toByteVector.foreach { b =>
          result = (result << 8) | (0x0ff & b)
        }
      } else {
        var i = 0
        b.toByteVector.foreach { b =>
          result = result | ((0x0ff & b) << (8 * i))
          i += 1
        }
      }
      if (mod != 0) result = result >>> (8 - mod)
      // Sign extend if necessary
      if (signed && bits != 32 && ((1 << (bits - 1)) & result) != 0) {
        val toShift = 32 - bits
        result = (result << toShift) >> toShift
      }
      result.right
    }

  override def toString = description + " codec"
}
