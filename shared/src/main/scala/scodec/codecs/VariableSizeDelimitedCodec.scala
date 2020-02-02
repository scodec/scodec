package scodec
package codecs

import scodec.bits.BitVector

private[codecs] final class VariableSizeDelimitedCodec[A](
    delimiterCodec: Codec[Unit],
    valueCodec: Codec[A],
    multipleValueSize: Long = 0L
) extends Codec[A] {

  val delimiter = delimiterCodec.encode(()).require
  val segmentSize = valueCodec.sizeBound.exact.getOrElse(multipleValueSize)

  require(
    segmentSize > 0,
    "valueCodec must have an exact sizeBound or you need to specify multipleValueSize"
  )

  require(
    delimiterCodec.sizeBound.lowerBound >= segmentSize,
    "delimiterCodec cannot be smaller than the sizeBound of the valueCodec"
  )

  def sizeBound = delimiterCodec.sizeBound.atLeast

  override def encode(a: A) =
    for {
      encA <- valueCodec.encode(a)
    } yield encA ++ delimiter

  override def decode(buffer: BitVector) = {
    val index = findDelimiterIndex(buffer)
    if (index != -1) {
      val valueBuffer = buffer.take(index)
      val remainder = buffer.drop(index + delimiter.size)
      valueCodec.decode(valueBuffer).map(decodeResult => decodeResult.mapRemainder(_ ++ remainder))
    } else {
      Attempt.failure(Err(s"expected delimiter $delimiterCodec"))
    }
  }

  private def findDelimiterIndex(buffer: BitVector): Long = {
    ???
    // var offset = 0L
    // do {
    //   if (buffer.drop(offset).startsWith(delimiter)) {
    //     return offset
    //   }
    //   offset += segmentSize
    // } while (offset < buffer.size)
    // -1
  }

  override def toString = s"VariableSizeDelimited($delimiterCodec, $valueCodec, $multipleValueSize)"
}
