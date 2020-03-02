package scodec
package codecs

import java.util.zip.{DataFormatException, Inflater}

import scodec.bits.{BitVector, ByteVector}

private[scodec] class ZlibCodec[A](
    codec: Codec[A],
    level: Int,
    strategy: Int,
    nowrap: Boolean,
    chunkSize: Int
) extends Codec[A] {

  def sizeBound = SizeBound.unknown

  def encode(a: A) =
    codec.encode(a).map(b => b.deflate(level, strategy, nowrap, chunkSize))

  def decode(b: BitVector) =
    inflate(b, chunkSize).fold(
      e => Attempt.failure(Err(e.getMessage)),
      bb => codec.decode(bb.value.bits).map(_.mapRemainder(_ => bb.remainder))
    )

  private def inflate(
      b: BitVector,
      chunkSize: Int
  ): Either[DataFormatException, DecodeResult[ByteVector]] =
    if (b.isEmpty) Right(DecodeResult(b.bytes, b))
    else {
      val arr = b.bytes.toArray

      val inflater = new Inflater(false)
      try {
        inflater.setInput(arr)
        try {
          val buffer = new Array[Byte](chunkSize.min(arr.length))
          def loop(acc: ByteVector): ByteVector =
            if (inflater.finished || inflater.needsInput) acc
            else {
              val count = inflater.inflate(buffer)
              loop(acc ++ ByteVector(buffer, 0, count))
            }
          val inflated = loop(ByteVector.empty)
          if (inflater.finished) Right(DecodeResult(inflated, b.drop(inflater.getBytesRead * 8)))
          else
            Left(
              new DataFormatException(
                "Insufficient data -- inflation reached end of input without completing inflation - " + inflated
              )
            )
        } catch {
          case e: DataFormatException => Left(e)
        }
      } finally {
        inflater.end()
      }
    }
}
