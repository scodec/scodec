package scodec

import Codecs._


class VariableSizeCodec[A](sizeCodec: Codec[Int], valueCodec: Codec[A]) extends Codec[A] {

  private val decoder = sizeCodec flatZip { sz => fixedSizeBits(sz, valueCodec) }

  override def encode(a: A) = for {
    encA <- valueCodec.encode(a)
    encSize <- sizeCodec.encode(encA.size).leftMap { e => s"[$a] is too long to be encoded: $e" }
  } yield encSize ++ encA

  override def decode(buffer: BitVector) =
    decoder.decode(buffer).map { case (rest, (sz, value)) => (rest, value) }
}
