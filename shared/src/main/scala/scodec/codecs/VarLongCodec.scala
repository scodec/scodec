package scodec
package codecs

import java.nio.{ByteBuffer, ByteOrder}

import scodec.bits.ByteOrdering.BigEndian
import scodec.bits.{BitVector, ByteOrdering}

import scala.annotation.tailrec

private[codecs] final class VarLongCodec(ordering: ByteOrdering) extends Codec[Long] {
  import VarLongCodec._

  override def sizeBound = SizeBound.bounded(1L, 9L)

  override def encode(i: Long) = {
    require(i >= 0, "VarLong cannot encode negative longs")
    val buffer = ByteBuffer.allocate(9).order(ByteOrder.BIG_ENDIAN)
    val encoder = if (ordering == BigEndian) BEEncoder else LEEncoder
    val written = encoder(i, buffer)
    buffer.flip()
    val relevantBits = BitVector.view(buffer).take(written.toLong)
    val bits = if (ordering == BigEndian) relevantBits else relevantBits.reverseByteOrder
    Attempt.successful(bits)
  }

  override def decode(buffer: BitVector) = {
    val decoder = if (ordering == BigEndian) BEDecoder else LEDecoder
    decoder(buffer)
  }

  override def toString = "variable-length integer"

  private val BEEncoder = (value: Long, buffer: ByteBuffer) => runEncodingBE(value, buffer, 8)

  private val LEEncoder = (value: Long, buffer: ByteBuffer) => runEncodingLE(value, buffer, 8, 0)

  @tailrec
  private def runEncodingBE(value: Long, buffer: ByteBuffer, size: Int): Int =
    if ((value & MoreBytesMask) != 0) {
      buffer.put((value & RelevantDataBits | MostSignificantBit).toByte)
      runEncodingBE(value >>> BitsPerByte, buffer, size + 8)
    } else {
      buffer.put(value.toByte)
      size
    }

  @tailrec
  private def runEncodingLE(value: Long, buffer: ByteBuffer, size: Int, msbMask: Int): Int =
    if ((value & MoreBytesMask) != 0) {
      buffer.put((value & RelevantDataBits | msbMask).toByte)
      runEncodingLE(value >>> BitsPerByte, buffer, size + 8, MostSignificantBit)
    } else {
      buffer.put((value | msbMask).toByte)
      size
    }

  private val BEDecoder = (buffer: BitVector) =>
    runDecodingBE(buffer, MostSignificantBit.toByte, 0L, 0)

  private val LEDecoder = (buffer: BitVector) =>
    runDecodingLE(buffer, MostSignificantBit.toByte, 0L)

  @tailrec
  private def runDecodingBE(
      buffer: BitVector,
      byte: Byte,
      value: Long,
      shift: Int
  ): Attempt[DecodeResult[Long]] =
    if ((byte & MostSignificantBit) != 0) {
      if (buffer.sizeLessThan(8L)) {
        Attempt.failure(Err.InsufficientBits(8L, buffer.size, Nil))
      } else {
        val nextByte = buffer.take(8L).toByte(false)
        val nextValue = value | (nextByte & RelevantDataBits) << shift
        runDecodingBE(buffer.drop(8L), nextByte, nextValue, shift + BitsPerByte)
      }
    } else {
      Attempt.successful(DecodeResult(value, buffer))
    }

  @tailrec
  private def runDecodingLE(
      buffer: BitVector,
      byte: Byte,
      value: Long
  ): Attempt[DecodeResult[Long]] =
    if ((byte & MostSignificantBit) != 0) {
      if (buffer.sizeLessThan(8L)) {
        Attempt.failure(Err.InsufficientBits(8L, buffer.size, Nil))
      } else {
        val nextByte = buffer.take(8L).toByte(false)
        val nextValue = (value << BitsPerByte) | (nextByte & RelevantDataBits)
        runDecodingLE(buffer.drop(8L), nextByte, nextValue)
      }
    } else {
      Attempt.successful(DecodeResult(value, buffer))
    }
}
object VarLongCodec {
  private val RelevantDataBits = 0x7FL
  private val MoreBytesMask = ~RelevantDataBits.toInt
  private val MostSignificantBit = 0x80
  private val BitsPerByte = 7
}
