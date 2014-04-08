package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._
import scalaz.syntax.std.option._

import java.nio.{ ByteBuffer, ByteOrder }

import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

private[codecs] final class IntCodec(bits: Int, signed: Boolean, ordering: ByteOrdering) extends Codec[Int] {

  require(bits > 0 && bits <= (if (signed) 32 else 31), "bits must be in range [1, 32] for signed and [1, 31] for unsigned")

  val MaxValue = (1 << (if (signed) (bits - 1) else bits)) - 1
  val MinValue = if (signed) -(1 << (bits - 1)) else 0

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} integer"

  override def encode(i: Int) = {
    if (i > MaxValue) {
      \/.left(s"$i is greater than maximum value $MaxValue for $description")
    } else if (i < MinValue) {
      \/.left(s"$i is less than minimum value $MinValue for $description")
    } else {
      \/.right(BitVector.fromInt(i, bits, ordering))
    }
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(e) => \/.left(e)
      case Right(b) => \/.right((buffer.drop(bits), b.toInt(signed, ordering)))
    }

  override def toString = description
}
