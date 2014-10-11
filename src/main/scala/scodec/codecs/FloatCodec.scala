package scodec
package codecs

import java.nio.ByteBuffer

import scalaz.\/
import scalaz.syntax.std.either._
import scodec.bits.{ BitVector, ByteOrdering }

private[codecs] final class FloatCodec(ordering: ByteOrdering) extends Codec[Float] {

  private val byteOrder = ordering.toJava

  override def encode(value: Float) = {
    val buffer = ByteBuffer.allocate(4).order(ordering.toJava).putFloat(value)
    buffer.flip()
    \/.right(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(32) match {
      case Left(e) => \/.left(Err.insufficientBits(32, buffer.size))
      case Right(b) => \/.right((buffer.drop(32), ByteBuffer.wrap(b.toByteArray).order(byteOrder).getFloat))
    }
}
