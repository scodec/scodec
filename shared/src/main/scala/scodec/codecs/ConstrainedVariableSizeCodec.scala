package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class ConstrainedVariableSizeCodec[A](
    sizeCodec: Codec[Long],
    valueCodec: Codec[A],
    minSize: Long,
    maxSize: Long
) extends Codec[A] {
  require(minSize < maxSize)
  require(minSize > -1)

  val minSizeBits = minSize * 8
  val maxSizeBits = maxSize * 8

  private def checkBoundaries(sz: Long) = minSizeBits <= sz && sz <= maxSizeBits

  private val decoder = sizeCodec.flatMap { sz =>
    if (checkBoundaries(sz)) {
      fixedSizeBits(sz, valueCodec).complete
    } else {
      fail[A](Err(s"Size out of bounds: $minSizeBits <= $sz <= $maxSizeBits is not true"))
    }
  }

  def sizeBound = sizeCodec.sizeBound.atLeast

  override def encode(a: A) = valueCodec.complete.encode(a).flatMap { enc =>
    val sz = enc.size

    if (checkBoundaries(sz))
      sizeCodec.encode(sz).map(_ ++ enc).mapErr(e => failMsg(a, e.messageWithContext))
    else
      Attempt.failure(Err(s"Size out of bounds: $minSizeBits <= $sz <= $maxSizeBits is not true"))

  }

  private def failMsg(a: A, msg: String): Err =
    Err.General(s"failed to encode size of [$a]: $msg", List("size"))

  override def decode(buffer: BitVector) =
    decoder.decode(buffer)

  override def toString = s"constrainedVariableSizeBits($sizeCodec, $valueCodec)"
}
