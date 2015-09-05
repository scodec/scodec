package scodec
package examples

import scodec.bits._
import scodec.codecs._

/**
 * Demonstrates a `List[A]` codec that:
 *  - separates list entries with a specified reserved byte
 *  - surrounds the list with a leading 0x22 and trailing 0x22
 *  - list entires may not contain the specified reserved byte
 */
class CustomDelimitedExample extends CodecSuite {

  def byteDelimited[A](value: Codec[A], delimiter: Byte): Codec[List[A]] = new Codec[List[A]] {
    def sizeBound = SizeBound.unknown
    def encode(as: List[A]) = as.foldLeft(Attempt.successful(BitVector.empty)) { (acc, a) =>
      for {
        x <- acc
        y <- value.encode(a)
        _ <- if (y.bytes.containsSlice(ByteVector(delimiter)))
               Attempt.failure(Err(s"encoded form of $a contained reserved delimiter $delimiter"))
             else
               Attempt.successful(())
      } yield x ++ BitVector(delimiter) ++ y
    }
    def decode(b: BitVector) = {
      def go(acc: List[A], remainder: BitVector): Attempt[DecodeResult[List[A]]] = {
        if (remainder.isEmpty) Attempt.successful(DecodeResult(acc.reverse, remainder))
        else {
          val nextValue = remainder.bytes.takeWhile(_ != delimiter).bits
          value.decode(nextValue) match {
            case Attempt.Successful(DecodeResult(a, newRemainder)) =>
              go(a :: acc, remainder.bytes.drop(nextValue.bytes.size + 1).bits)
            case f: Attempt.Failure => f
          }
        }
      }
      go(Nil, b)
    }
  }

  def quoted[A](inner: Codec[A]): Codec[A] = new Codec[A] {
    private val quote = BitVector(0x22)
    def sizeBound = inner.sizeBound + SizeBound.exact(16)
    def encode(a: A) = inner.encode(a).map { b => quote ++ b ++ quote }
    def decode(b: BitVector) = (for {
      _ <- constant(0x22)
      b <- Decoder.get
      untilEndQuote = b.bytes.takeWhile(_ != 0x22.toByte).bits
      _ <- Decoder.set(untilEndQuote)
      value <- inner
      _ <- Decoder.set(b.drop(untilEndQuote.size))
      _ <- constant(0x22)
    } yield value).decode(b)
  }

  "the quotes/byteDelimited codec" should {
    "decode a known good value" in {
      val codec = quoted(byteDelimited(codecs.bytes, 0x3a.toByte))
      val payload = hex"22053a613a93213a3af50320004290290060293a503a09783a362e35353935373a932122".bits
      val result = codec.decode(payload)
      result shouldBe Attempt.successful(DecodeResult(
        List(
          hex"05",
          hex"61",
          hex"9321",
          hex"",
          hex"f5032000429029006029",
          hex"50",
          hex"0978",
          hex"362e3535393537",
          hex"9321"
        ),
        BitVector.empty
      ))
    }
  }
}
