package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class VariableSizePrefixedCodec[A, B](
  sizeCodec: Codec[Long], prefixCodec: Codec[A], valueCodec: Codec[B], sizePadding: Long) extends Codec[(A, B)] {

  private val decoder = sizeCodec flatMap { sz => prefixCodec ~ fixedSizeBits(sz - sizePadding, valueCodec) }

  def sizeBound = sizeCodec.sizeBound.atLeast + prefixCodec.sizeBound

  override def encode(ab: (A, B)) = for {
    encA <- prefixCodec.encode(ab._1)
    encB <- valueCodec.encode(ab._2)
    encSize <- sizeCodec.encode(encB.size + sizePadding).mapErr { e => fail(ab._2, e.messageWithContext) }
  } yield encSize ++ encA ++ encB

  private def fail(b: B, msg: String): Err =
    Err(s"[$b] is too long to be encoded: $msg")

  override def decode(buffer: BitVector) =
    decoder.decode(buffer)

  override def toString = s"variableSizePrefixedBits($sizeCodec, $prefixCodec, $valueCodec)"
}

