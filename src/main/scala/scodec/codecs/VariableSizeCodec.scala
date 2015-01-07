package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class VariableSizeCodec[A](sizeCodec: Codec[Long], valueCodec: Codec[A], sizePadding: Long) extends Codec[A] {

  private val decoder = sizeCodec flatZip { sz => fixedSizeBits(sz - sizePadding, valueCodec) }

  override def encode(a: A) = for {
    encA <- valueCodec.encode(a)
    encSize <- sizeCodec.encode(encA.size + sizePadding).mapErr { e => fail(a, e.messageWithContext) }
  } yield encSize ++ encA

  private def fail(a: A, msg: String): Err =
    Err(s"[$a] is too long to be encoded: $msg")

  override def decode(buffer: BitVector) =
    decoder.decode(buffer).map { _ mapValue { case (sz, value) => value } }

  override def toString = s"variableSizeBits($sizeCodec, $valueCodec)"
}
