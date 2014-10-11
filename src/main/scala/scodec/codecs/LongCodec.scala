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
      \/.left(Err(s"$i is greater than maximum value $MaxValue for $description"))
    } else if (i < MinValue) {
      \/.left(Err(s"$i is less than minimum value $MinValue for $description"))
    } else {
      \/.right(BitVector.fromLong(i, bits, ordering))
    }
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(e) => \/.left(Err.insufficientBits(bits, buffer.size))
      case Right(b) => \/.right((buffer.drop(bits), b.toLong(signed, ordering)))
    }

  override def toString = description
}
