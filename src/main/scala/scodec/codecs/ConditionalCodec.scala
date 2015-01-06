package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ConditionalCodec[A](included: Boolean, codec: Codec[A]) extends Codec[Option[A]] {

  override def encode(a: Option[A]) = {
    a.filter { _ => included } match {
      case None => EncodeResult.successful(BitVector.empty)
      case Some(a) => codec.encode(a)
    }
  }

  override def decode(buffer: BitVector) = {
    if (included)
      codec.decode(buffer).map { result => Some(result) }
    else
      DecodeResult.successful(None, buffer)
  }

  override def toString = s"conditional($included, $codec)"

}
