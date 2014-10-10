package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.either._
import scalaz.syntax.std.option._

import java.nio.{ ByteBuffer, ByteOrder }

import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

private[codecs] final class ByteCodec(bits: Int, signed: Boolean) extends Codec[Byte] {

  require(bits > 0 && bits <= (if (signed) 8 else 7), "bits must be in range [1, 8] for signed and [1, 7] for unsigned")

  val MaxValue = ((1 << (if (signed) (bits - 1) else bits)) - 1).toByte
  val MinValue = (if (signed) -(1 << (bits - 1)) else 0).toByte

  private def description = s"$bits-bit ${if (signed) "signed" else "unsigned"} byte"

  override def encode(b: Byte) = {
    if (b > MaxValue) {
      \/.left(Err(s"$b is greater than maximum value $MaxValue for $description"))
    } else if (b < MinValue) {
      \/.left(Err(s"$b is less than minimum value $MinValue for $description"))
    } else {
      \/.right(BitVector.fromByte(b, bits))
    }
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(bits) match {
      case Left(e) => \/.left(Err.insufficientBits(bits, buffer.size))
      case Right(b) => \/.right((buffer.drop(bits), b.toByte(signed)))
    }

  override def toString = description
}
