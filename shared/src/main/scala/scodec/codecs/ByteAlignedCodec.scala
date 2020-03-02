package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ByteAlignedCodec[A](codec: Codec[A]) extends Codec[A] {

  private def padAmount(size: Long) = {
    val mod = size % 8
    if (mod == 0) 0 else 8 - mod
  }

  def sizeBound = {
    val sz = codec.sizeBound
    val lb = sz.lowerBound + padAmount(sz.lowerBound)
    val ub = sz.upperBound.map(ub => ub + padAmount(ub))
    SizeBound(lb, ub)
  }

  def encode(a: A) =
    codec.encode(a).map { enc =>
      val pad = padAmount(enc.size)
      if (pad == 0) enc
      else enc.padTo(enc.size + pad)
    }

  def decode(b: BitVector) =
    codec.decode(b).map { res =>
      val taken = b.size - res.remainder.size
      val pad = padAmount(taken)
      if (pad == 0) res
      else DecodeResult(res.value, res.remainder.drop(pad))
    }

  override def toString = s"byteAligned($codec)"
}
