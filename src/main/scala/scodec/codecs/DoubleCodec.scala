package scodec
package codecs

import java.nio.ByteBuffer

import scalaz.\/
import scalaz.syntax.std.either._
import scodec.bits.{ BitVector, ByteOrdering }

private[codecs] final class DoubleCodec(ordering: ByteOrdering) extends Codec[Double] {

  private val byteOrder = ordering.toJava

  override def encode(value: Double) = {
    val buffer = ByteBuffer.allocate(8).order(byteOrder).putDouble(value)
    buffer.flip()
    \/.right(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(64) match {
      case Left(e) => \/.left(Err.insufficientBits(64, buffer.size))
      case Right(b) => \/.right((buffer.drop(64), ByteBuffer.wrap(b.toByteArray).order(byteOrder).getDouble))
    }

  override def toString = "double"
}

