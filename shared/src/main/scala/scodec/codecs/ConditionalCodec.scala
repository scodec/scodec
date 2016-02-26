package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ConditionalCodec[A](included: Boolean, codec: => Codec[A]) extends Codec[Option[A]] {

  private lazy val evaluatedCodec = codec

  override def sizeBound = if (included) evaluatedCodec.sizeBound else SizeBound.exact(0)

  override def encode(a: Option[A]) = {
    a.filter { _ => included } match {
      case None => Attempt.successful(BitVector.empty)
      case Some(a) => evaluatedCodec.encode(a)
    }
  }

  override def decode(buffer: BitVector) = {
    if (included)
      evaluatedCodec.decode(buffer).map { _ map { result => Some(result) } }
    else
      Attempt.successful(DecodeResult(None, buffer))
  }

  override def toString = if(included) s"conditional(true, $evaluatedCodec)" else "conditional(false, ?)"

}
