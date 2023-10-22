package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ChecksumAppendCodec[V](
    target: Codec[V],
    bitSize: Int,
    compute: BitVector => BitVector,
    validate: Boolean,
    framing: BitVector => BitVector
) extends Codec[V]:

  def decode(bits: BitVector): Attempt[DecodeResult[V]] =
    val (noCrcBytes, maybeCrcBytes) = bits.splitAt(bits.size - bitSize)
    target.decode(noCrcBytes).flatMap { case DecodeResult(value, remainder) =>
      val valueBits = noCrcBytes.dropRight(remainder.size)
      val actualCrc = compute(framing(valueBits))
      val (expectedCrc, nextBits) = (remainder ++ maybeCrcBytes).splitAt(bitSize)
      if (!validate || expectedCrc == actualCrc) Attempt.successful(DecodeResult(value, nextBits))
      else Attempt.failure(ChecksumMismatch(valueBits, expectedCrc, actualCrc))
    }

  def encode(value: V): Attempt[BitVector] =
    target.encode(value).map { bits =>
      bits ++ compute(framing(bits))
    }

  def sizeBound: SizeBound = target.sizeBound + SizeBound.exact(bitSize)
