package scodec

import Codecs._
import scalaz.\/._

class VariableSizeCodec[A](sizeCodec: Codec[Int], valueCodec: Codec[A]) extends Codec[A] {

  private val decoder = sizeCodec flatZip { sz => fixedSizeBits(sz, valueCodec) }

  override def encode(a: A) = for {
    encA <- valueCodec.encode(a)
    encSize <- encA.intSize.map(n => sizeCodec.encode(n).leftMap { fail(a, n, _) })
                           .getOrElse(left(fail(a, encA.size, "")))
  } yield encSize ++ encA

  private def fail(a: A, n: Long, msg: String): String =
    s"[$a] is too long to be encoded: $n $msg"

  override def decode(buffer: BitVector) =
    decoder.decode(buffer).map { case (rest, (sz, value)) => (rest, value) }
}
