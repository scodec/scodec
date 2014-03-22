package scodec
package codecs

import java.nio.{ ByteBuffer, ByteOrder }

import scalaz.\/
import scalaz.syntax.std.either._
import scodec.bits.BitVector

private[codecs] final class DoubleCodec(bigEndian: Boolean) extends Codec[Double] {

  private val byteOrder = if (bigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN

  override def encode(value: Double) = {
    val buffer = ByteBuffer.allocate(8).order(byteOrder).putDouble(value)
    buffer.flip()
    \/.right(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(64) match {
      case Left(e) => \/.left(e)
      case Right(b) => \/.right((buffer.drop(64), ByteBuffer.wrap(b.toByteArray).order(byteOrder).getDouble))
    }
}

