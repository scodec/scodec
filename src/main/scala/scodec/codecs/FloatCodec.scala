package scodec
package codecs

import java.nio.ByteBuffer

import scodec.bits.{ BitVector, ByteOrdering }

private[codecs] final class FloatCodec(ordering: ByteOrdering) extends Codec[Float] {

  private val byteOrder = ordering.toJava

  override def encode(value: Float) = {
    val buffer = ByteBuffer.allocate(4).order(ordering.toJava).putFloat(value)
    buffer.flip()
    EncodeResult.successful(BitVector.view(buffer))
  }

  override def decode(buffer: BitVector) =
    buffer.acquire(32) match {
      case Left(e) => DecodeResult.failure(Err.insufficientBits(32, buffer.size))
      case Right(b) => DecodeResult.successful(ByteBuffer.wrap(b.toByteArray).order(byteOrder).getFloat, buffer.drop(32))
    }

  override def toString = "float"
}
