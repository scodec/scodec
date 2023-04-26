package scodec
package codecs

import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.SizeBound
import scodec.bits.BitVector
import scodec.codecs.ChecksumMismatch

final class ChecksumRightCodec[V](inner: Codec[V], bitSize: Int, compute: BitVector => BitVector)
    extends Codec[V]:

  def decode(bits: BitVector): Attempt[DecodeResult[V]] =
    val (valueBits, expectedCrc) = bits.splitAt(bits.size - bitSize)
    inner.decode(valueBits).flatMap { result =>
      val actualCrc = compute(valueBits)
      if (actualCrc == expectedCrc) Attempt.successful(result)
      else Attempt.failure(ChecksumMismatch(valueBits, expectedCrc, actualCrc))
    }

  def encode(value: V): Attempt[BitVector] =
    inner.encode(value).map { result =>
      result ++ compute(result)
    }

  def sizeBound: SizeBound = inner.sizeBound + SizeBound.exact(bitSize)
