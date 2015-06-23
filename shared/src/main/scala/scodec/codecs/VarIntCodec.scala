package scodec
package codecs

import scodec.bits.{BitVector, ByteOrdering}

private[codecs] final class VarIntCodec(ordering: ByteOrdering) extends Codec[Int] {
  private[this] val long = new VarLongCodec(ordering).xmap(_.toInt, VarIntCodec.toPositiveLong)

  override def sizeBound =
    SizeBound.bounded(1L, 5L)

  override def encode(i: Int) =
    long.encode(i)

  override def decode(buffer: BitVector) =
    long.decode(buffer)

  override def toString = "variable-length integer"
}
object VarIntCodec {
  private val NegativeIntSignBit = Int.MaxValue.toLong + 1L

  // toLong left-pads with `1` if the int is negative which cannot be encoded by
  // the VarLongCodec. This pads negative ints with `0` instead.
  private val toPositiveLong = (i: Int) =>
    if (i >= 0) i.toLong else (i & Int.MaxValue) | NegativeIntSignBit
}
