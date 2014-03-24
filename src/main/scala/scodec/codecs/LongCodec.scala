package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._
import scalaz.syntax.std.option._

import java.nio.{ ByteBuffer, ByteOrder }

import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

private[codecs] final class LongCodec(bits: Int, signed: Boolean, ordering: ByteOrdering) extends Codec[Long] {

  require(bits > 0 && bits <= (if (signed) 64 else 63), "bits must be in range [1, 64] for signed and [1, 63] for unsigned")

  val MaxValue = (1L << (if (signed) (bits - 1) else bits)) - 1
  val MinValue = if (signed) -(1L << (bits - 1)) else 0

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} integer"

  override def encode(i: Long) = {
    if (i > MaxValue) {
      \/.left(s"$i is greater than maximum value $MaxValue for $description")
    } else if (i < MinValue) {
      \/.left(s"$i is less than minimum value $MinValue for $description")
    } else {
      val buffer = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(i)
      buffer.flip()
      val relevantBits = (BitVector.view(buffer) << (64 - bits)).take(bits)
      \/.right(if (ordering == ByteOrdering.BigEndian) relevantBits else relevantBits.reverseByteOrder)
    }
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(e) => \/.left(e)
      case Right(b) =>
        val mod = bits % 8
        var result = 0L
        ordering match {
          case ByteOrdering.BigEndian =>
            @annotation.tailrec
            def go(bv: ByteVector): Unit =
              if (bv.nonEmpty) {
                result = (result << 8) | (0x0ffL & bv.head)
                go(bv.tail)
              }
            go(b.toByteVector)
          case ByteOrdering.LittleEndian =>
            @annotation.tailrec
            def go(bv: ByteVector, i: Int): Unit =
              if (bv.nonEmpty) {
                result = result | ((0x0ffL & bv.head) << (8 * i))
                go(bv.tail, i + 1)
              }
            go(b.toByteVector, 0)
        }
        if (mod != 0) result = result >>> (8 - mod)
        // Sign extend if necessary
        if (signed && bits != 64 && ((1 << (bits - 1)) & result) != 0) {
          val toShift = 64 - bits
          result = (result << toShift) >> toShift
        }
        \/.right((buffer.drop(bits), result))
    }

  override def toString = description
}
