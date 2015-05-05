package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class LimitedSizeCodec[A](limit: Long, codec: Codec[A]) extends Codec[A] {

  def sizeBound = {
    val sz = codec.sizeBound
    val ub = sz.upperBound.map { _ min limit }.getOrElse(limit)
    SizeBound.bounded(sz.lowerBound, ub)
  }

  def encode(a: A) = {
    codec.encode(a) flatMap { enc =>
      if (enc.size > limit)
        Attempt.failure(Err(s"[$a] requires ${enc.size} bits but field is limited to $limit bits"))
      else Attempt.successful(enc)
    }
  }

  def decode(b: BitVector) = {
    codec.decode(b.take(limit)) map { res =>
      DecodeResult(res.value, res.remainder ++ b.drop(limit))
    }
  }

  override def toString = s"limitedSizeBits($limit, $codec)"
}


