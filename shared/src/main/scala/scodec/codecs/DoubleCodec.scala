package scodec
package codecs

import java.nio.ByteBuffer

import scodec.bits.{BitVector, ByteOrdering}

private[codecs] final class DoubleCodec(ordering: ByteOrdering) extends Codec[Double] {

  private val byteOrder = ordering.toJava

  override def sizeBound = SizeBound.exact(64)

  override def encode(value: Double) = {
    val buffer = ByteBuffer.allocate(8).order(byteOrder).putDouble(value).nn
    buffer.flip()
    Attempt.successful(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(64) match {
      case Left(_) => Attempt.failure(Err.insufficientBits(64, buffer.size))
      case Right(b) =>
        Attempt.successful(
          DecodeResult(ByteBuffer.wrap(b.toByteArray).order(byteOrder).getDouble, buffer.drop(64))
        )
    }

  override def toString = "double"
}
