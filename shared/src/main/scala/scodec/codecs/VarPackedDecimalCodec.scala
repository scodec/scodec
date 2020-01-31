package scodec
package codecs

import scodec.bits.BitVector

import scala.annotation.tailrec

private[codecs] object VarPackedDecimalCodec extends Codec[Long] {
  override def sizeBound = SizeBound.bounded(1L, 9L)

  def encode(i: Long): Attempt[BitVector] =
    if (i < 0) Attempt.failure(Err("VarPackedDecimal cannot encode negative longs"))
    else runEncoding(i, Attempt.successful(BitVector.empty))

  def decode(buffer: BitVector): Attempt[DecodeResult[Long]] = runDecoding(buffer, 0L)

  @tailrec
  private def runDecoding(buffer: BitVector, value: Long): Attempt[DecodeResult[Long]] =
    if (buffer.isEmpty) Attempt.successful(DecodeResult(value, buffer))
    else if (buffer.sizeLessThan(4)) Attempt.failure(Err.InsufficientBits(4L, buffer.size, Nil))
    else runDecoding(buffer.drop(4), value * 10 + buffer.take(4).toInt(false))

  @tailrec
  private def runEncoding(value: Long, acc: Attempt[BitVector]): Attempt[BitVector] =
    if (value < 10L) uint(4).encode(value.toInt).flatMap(x => acc.map(x ++ _))
    else runEncoding(value / 10, uint(4).encode((value % 10).toInt).flatMap(x => acc.map(x ++ _)))
}
