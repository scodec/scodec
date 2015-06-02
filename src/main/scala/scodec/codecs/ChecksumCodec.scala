package scodec
package codecs

import scodec.bits.{BitVector, ByteVector}

/**
 * Codec that appends/verifies a checksum.
 */
private[codecs] final class ChecksumCodec[A](target: Codec[A], checksum: Encoder[BitVector], rangeSize: Decoder[Long], rangePadding: Long) extends Codec[A] {

  def decode(bits: BitVector): Attempt[DecodeResult[A]] =
    computeThen(bits)(
      (range, expected, remainder) => remainder.consumeThen(expected.size)(
        _ => Attempt.failure(Err.insufficientBits(expected.size, remainder.size)),
        (actual, rest) =>
          if (actual == expected) target.decode(range ++ rest)
          else Attempt.failure(ChecksumCodec.Mismatch(range, expected, actual))))

  def encode(value: A): Attempt[BitVector] =
    target.encode(value) flatMap (
      bits => computeThen(bits)((range, sum, remainder) => Attempt.successful(range ++ sum ++ remainder)))

  private def computeThen[B](bits: BitVector)(f: (BitVector, BitVector, BitVector) => Attempt[B]) =
    rangeSize.decode(bits) flatMap (
      size => bits.consumeThen(size.value + rangePadding)(
        _ => Attempt.failure(Err.insufficientBits(size.value + rangePadding, bits.size)),
        (range, remainder) => checksum.encode(range) flatMap (sum => f(range, sum, remainder))))

  def sizeBound: SizeBound = target.sizeBound + checksum.sizeBound
}

object ChecksumCodec {

  def xorBits(length: Long) = Xor(length)

  def xor(length: Int) = Xor(8L * length).contramap[ByteVector](_.bits)

  case class Xor(length: Long) extends Encoder[BitVector] {
    val init = BitVector.fill(length)(high = false)

    def encode(value: BitVector): Attempt[BitVector] =
      Attempt.successful(value.grouped(length).fold(init)(_ xor _))

    def sizeBound: SizeBound = SizeBound.exact(length)
  }

  case class Mismatch(bits: BitVector, expected: BitVector, actual: BitVector, context: List[String] = Nil) extends Err {

    def message: String = s"checksum mismatch for bits: $bits, expected: $expected, actual: $actual"

    def pushContext(ctx: String): Err = copy(context = ctx :: context)
  }

}