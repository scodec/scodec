package scodec
package codecs

import scalaz.\/
import scalaz.syntax.id._
import scalaz.syntax.std.either._
import scodec.bits.BitVector

private[codecs] final class EitherCodec[L, R](indicatorCodec: Codec[Boolean], leftCodec: Codec[L], rightCodec: Codec[R]) extends Codec[L \/ R] {

  override def encode(value: L \/ R) = for {
    encIndicator <- indicatorCodec.encode(value.isRight)
    encValue <- value.fold(leftCodec.encode, rightCodec.encode)
  } yield encIndicator ++ encValue

  override def decode(buffer: BitVector) = (for {
    isRight <- DecodingContext(indicatorCodec.decode)
    value <- (
      if (isRight) DecodingContext(rightCodec.decode).map(\/.right)
      else DecodingContext(leftCodec.decode).map(\/.left)
    )
  } yield value).run(buffer)
}

