package scodec
package codecs

import java.nio.ByteBuffer

import scodec.bits.{BitVector, ByteOrdering}

private[codecs] final class FloatCodec(ordering: ByteOrdering) extends Codec[Float] {

  private val byteOrder = ordering.toJava

  override def sizeBound = SizeBound.exact(32)

  override def encode(value: Float) = {
    val buffer = ByteBuffer.allocate(4).order(ordering.toJava).putFloat(value).nn
    buffer.flip()
    Attempt.successful(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(32) match {
      case Left(_) => Attempt.failure(Err.insufficientBits(32, buffer.size))
      case Right(b) =>
        Attempt.successful(
          DecodeResult(ByteBuffer.wrap(b.toByteArray).order(byteOrder).getFloat, buffer.drop(32))
        )
    }

  override def toString = "float"
}
