package scodec.codecs

import scodec._
import scodec.bits.BitVector

import scala.language.higherKinds

import scodec.compat._

/**
  * A trait that enables custom handling for encoding/decoding sequences.
  */
sealed trait MultiplexedCodec {

  def sizeBound: SizeBound = SizeBound.unknown

  /**
    * Encodes all elements of the specified sequence and combines the results using `mux`, or returns the first encountered error.
    *
    * @param enc element encoder
    * @param mux multiplexing function
    * @param seq elements to encode
    * @return
    */
  final def encode[A](enc: Encoder[A], mux: (BitVector, BitVector) => BitVector)(
      seq: collection.immutable.Seq[A]
  ): Attempt[BitVector] = {
    val buf = new collection.mutable.ArrayBuffer[BitVector](seq.size)
    seq.foreach { a =>
      enc.encode(a) match {
        case Attempt.Successful(aa) => buf += aa
        case Attempt.Failure(err)   => return Attempt.failure(err.pushContext(buf.size.toString))
      }
    }
    def merge(offset: Int, size: Int): BitVector = size match {
      case 0 => BitVector.empty
      case 1 => buf(offset)
      case n =>
        val half = size / 2
        mux(merge(offset, half), merge(offset + half, half + (if (size % 2 == 0) 0 else 1)))
    }
    Attempt.successful(merge(0, buf.size))
  }

  /**
    * Repeatedly decodes values of type `A` and returns a collection of the specified type.
    * Uses `deMux` repeatedly to obtain the stream of vectors to decode to a value of type `A`.
    * Terminates when the next stream to decode is empty or upon first decoding error.
    *
    * Note: For large sequences, it may be necessary to compact bits in `deMux`.
    *
    * @param dec element decoder
    * @param deMux returns `(next, rest)` tuples where `next` is input to `dec` yielding `(value, remainder)` and `remainder ++ rest` is the next input to `deMux`
    * @param buffer input bits
    * @return
    */
  final def decode[F[_], A](dec: Decoder[A], deMux: BitVector => (BitVector, BitVector))(
      buffer: BitVector
  )(implicit cbf: Factory[A, F[A]]): Attempt[DecodeResult[F[A]]] = {
    val builder = cbf.newBuilder
    var temp = deMux(buffer)
    var count = 0
    var error: Option[Err] = None
    while (temp._1.nonEmpty) {
      dec.decode(temp._1) match {
        case Attempt.Successful(DecodeResult(value, remainder)) =>
          builder += value
          count += 1
          temp = deMux(remainder ++ temp._2)
        case Attempt.Failure(err) =>
          error = Some(err.pushContext(count.toString))
          temp = (BitVector.empty, BitVector.empty)
      }
    }
    Attempt.fromErrOption(error, DecodeResult(builder.result(), temp._2))
  }
}

object DeMultiplexer {

  /**
    * Returns a `(next, rest)` tuple where `next` is the prefix of the input preceding the first occurrence of `delimiter`.
    *
    * Note: The search for `delimiter` is performed at `delimiter` sized intervals.
    *
    * @param bits the input bits
    * @param delimiter the separator bits
    * @return
    */
  def delimited(bits: BitVector, delimiter: BitVector): (BitVector, BitVector) =
    delimited(bits, delimiter, 0)

  private def delimited(
      bits: BitVector,
      delimiter: BitVector,
      start: Long
  ): (BitVector, BitVector) =
    bits.indexOfSlice(delimiter, start) match {
      case -1                             => (bits, BitVector.empty)
      case i if (i % delimiter.size) == 0 => (bits.take(i), bits.drop(i + delimiter.size))
      case i                              => delimited(bits, delimiter, i + delimiter.size)
    }
}

private[codecs] class VectorMultiplexedCodec[A](
    mux: (BitVector, BitVector) => BitVector,
    deMux: BitVector => (BitVector, BitVector),
    codec: Codec[A]
) extends Codec[Vector[A]]
    with MultiplexedCodec {
  def encode(value: Vector[A]): Attempt[BitVector] = encode(codec, mux)(value)

  def decode(bits: BitVector): Attempt[DecodeResult[Vector[A]]] =
    decode[Vector, A](codec, deMux)(bits)

  override def toString: String = s"vectorMultiplexed($codec, $mux, $deMux)"
}

private[codecs] class ListMultiplexedCodec[A](
    mux: (BitVector, BitVector) => BitVector,
    deMux: BitVector => (BitVector, BitVector),
    codec: Codec[A]
) extends Codec[List[A]]
    with MultiplexedCodec {
  def encode(value: List[A]): Attempt[BitVector] = encode(codec, mux)(value)

  def decode(bits: BitVector): Attempt[DecodeResult[List[A]]] = decode[List, A](codec, deMux)(bits)

  override def toString: String = s"listMultiplexed($codec, $mux, $deMux)"
}
