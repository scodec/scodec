package scodec
package codecs

import java.nio.{ ByteBuffer, ByteOrder }

import scalaz.syntax.id._
import scalaz.syntax.std.either._
import scodec.bits.BitVector

private[codecs] final class DoubleCodec(bigEndian: Boolean) extends Codec[Double] {

  private val byteOrder = if (bigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN

  override def encode(value: Double) =
    BitVector(ByteVector.view(ByteBuffer.allocate(8).order(byteOrder).putDouble(value).array)).right

  override def decode(buffer: BitVector) =
    buffer.consume(64) { b =>
      Right(ByteBuffer.wrap(b.toByteArray).order(byteOrder).getDouble)
    }.disjunction
}

