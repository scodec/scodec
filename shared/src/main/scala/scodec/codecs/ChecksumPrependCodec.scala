package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ChecksumPrependCodec[V](
    target: Codec[V],
    bitSize: Int,
    compute: BitVector => BitVector,
    validate: Boolean,
    framing: BitVector => BitVector
) extends Codec[V]:

  private val codec = bits(bitSize) :: target

  def decode(bits: BitVector): Attempt[DecodeResult[V]] =
    codec.decode(bits).flatMap { case DecodeResult((expectedCrc, value), remainder) =>
      val valueBits = bits.drop(bitSize).dropRight(remainder.size)
      val actualCrc = compute(framing(valueBits))
      if (!validate || expectedCrc == actualCrc) Attempt.successful(DecodeResult(value, remainder))
      else Attempt.failure(ChecksumMismatch(valueBits, expectedCrc, actualCrc))
    }

  def encode(value: V): Attempt[BitVector] =
    target.encode(value).map { bits =>
      compute(framing(bits)) ++ bits
    }

  def sizeBound: SizeBound = target.sizeBound + SizeBound.exact(bitSize)
