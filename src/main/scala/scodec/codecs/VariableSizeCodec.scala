package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class VariableSizeCodec[A](sizeCodec: Codec[Long], valueCodec: Codec[A], sizePadding: Long) extends Codec[A] {

  private val decoder = sizeCodec flatMap { sz => fixedSizeBits(sz - sizePadding, valueCodec) }

  def sizeBound = sizeCodec.sizeBound.atLeast

  override def encode(a: A) = for {
    encA <- valueCodec.encode(a)
    encSize <- sizeCodec.encode(encA.size + sizePadding).mapErr { e => fail(a, e.messageWithContext) }
  } yield encSize ++ encA

  private def fail(a: A, msg: String): Err =
    Err(s"[$a] is too long to be encoded: $msg")

  override def decode(buffer: BitVector) =
    decoder.decode(buffer)

  override def toString = s"variableSizeBits($sizeCodec, $valueCodec)"
}
