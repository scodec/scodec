package scodec
package codecs

import java.nio.{ ByteBuffer, ByteOrder }

import scalaz.\/
import scalaz.syntax.std.either._
import scodec.bits.BitVector

private[codecs] final class FloatCodec(bigEndian: Boolean) extends Codec[Float] {

  private val byteOrder = if (bigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN

  override def encode(value: Float) = {
    val buffer = ByteBuffer.allocate(4).order(byteOrder).putFloat(value)
    buffer.flip()
    \/.right(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(32) match {
      case Left(e) => \/.left(e)
      case Right(b) => \/.right((buffer.drop(32), ByteBuffer.wrap(b.toByteArray).order(byteOrder).getFloat))
    }
}
