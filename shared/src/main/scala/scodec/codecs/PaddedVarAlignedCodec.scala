package scodec
package codecs

import scodec.{Codec, Err}
import scodec.bits.BitVector

class PaddedVarAlignedCodec[A](sizeCodec: Codec[Long], valueCodec: Codec[A], multipleForPadding: Long) extends Codec[A] {
  
  def calculatePadding(i: Long): Long = (multipleForPadding - (i % multipleForPadding)) % multipleForPadding

  val decoder = for {
    size <- sizeCodec
    a <- fixedSizeBits(size, valueCodec)
    _ <- ignore(calculatePadding(size))
  } yield a

  def sizeBound = sizeCodec.sizeBound.atLeast

  override def encode(a: A) = for {
    encA <- valueCodec.encode(a)
    padsize = calculatePadding(encA.size)
    encSize <- sizeCodec.encode(encA.size).mapErr { e => fail(a, e.messageWithContext) }
  } yield encSize ++ encA ++ BitVector.fill(padsize)(false)

  private def fail(a: A, msg: String): Err =
    Err.General(s"failed to encode size of [$a]: $msg", List("size"))

  override def decode(buffer: BitVector) =
    decoder.decode(buffer)
}
