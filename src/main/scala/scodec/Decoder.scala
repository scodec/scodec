package scodec

import scala.language.higherKinds

import scalaz.{ \/, \/-, -\/, Monad, Monoid }
import scalaz.syntax.std.option._

import scodec.bits.BitVector

/**
 * Supports decoding a value of type `A` from a `BitVector`.
 *
 * @groupname primary Primary Members
 * @groupprio primary 0
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 10
 */
trait Decoder[+A] { self =>

  /**
   * Attempts to decode a value of type `A` from the specified bit vector.
   *
   * @param bits bits to decode
   * @return error if value could not be decoded or the remaining bits and the decoded value
   * @group primary
   */
  def decode(bits: BitVector): String \/ (BitVector, A)

  /**
   * Attempts to decode a value of type `A` from the specified bit vector and discards any
   * remaining bits.
   *
   * @param bits bits to decode
   * @return error if value could not be decoded or the decoded value
   * @group primary
   */
  final def decodeValue(bits: BitVector): String \/ A =
    decode(bits) map { case (rem, value) => value }

    /**
   * Decodes a value of type `A` from the specified bit vector and discards any
   * remaining bits, throwing an `IllegalArgumentException` if an error occurs.
   *
   * @param bits bits to decode
   * @return the decoded value
   * @throws IllegalArgumentException if a decoding error occurs
   * @group primary
   */
  final def decodeValidValue(bits: BitVector): A =
    decodeValue(bits) valueOr { err => throw new IllegalArgumentException(err) }

  /**
   * Converts this decoder to a `Decoder[B]` using the supplied `A => B`.
   * @group combinators
   */
  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def decode(bits: BitVector) = self.decode(bits) map { case (rem, a) => (rem, f(a)) }
  }

  /**
   * Converts this decoder to a `Decoder[B]` using the supplied `A => Decoder[B]`.
   * @group combinators
   */
  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def decode(bits: BitVector) = self.decode(bits) flatMap { case (rem, a) => f(a).decode(rem) }
  }

  /**
   * Converts this decoder to a new decoder that fails decoding if there are remaining bits.
   * @group combinators
   */
  def complete: Decoder[A] = new Decoder[A] {
    def decode(bits: BitVector) = self.decode(bits) flatMap { case r @ (rem, a) =>
      if (rem.isEmpty) \/.right(r) else {
        \/.left {
          val max = 512
          if (rem.sizeLessThan(max + 1)) {
            val preview = rem.take(max)
            s"${preview.size} bits remaining: 0x${preview.toHex}"
          } else s"more than $max bits remaining"
        }
      }
    }
  }
}

/** Provides functions for working with decoders. */
trait DecoderFunctions {

  /** Decodes the specified bit vector in to a value of type `A` using an implicitly available codec. */
  final def decode[A: Decoder](bits: BitVector): String \/ (BitVector, A) = Decoder[A].decode(bits)

  /**
   * Decodes the specified bit vector in to a value of type `A` using an implicitly available
   * codec and discards the remaining bits.
   */
  final def decodeValue[A: Decoder](bits: BitVector): String \/ A = Decoder[A].decodeValue(bits)

  /**
   * Decodes the specified bit vector in to a value of type `A` using an implicitly available
   * codec and discards the remaining bits or throws an `IllegalArgumentException` if decoding
   * fails.
   */
  final def decodeValidValue[A: Decoder](bits: BitVector): A = Decoder[A].decodeValidValue(bits)

  /** Decodes a tuple `(A, B)` by first decoding `A` and then using the remaining bits to decode `B`. */
  final def decodeBoth[A, B](decA: Decoder[A], decB: Decoder[B])(buffer: BitVector): String \/ (BitVector, (A, B)) =
    decodeBothCombine(decA, decB)(buffer) { (a, b) => (a, b) }

  /** Decodes a `C` by first decoding `A` and then using the remaining bits to decode `B`, then applying the decoded values to the specified function to generate a `C`. */
  final def decodeBothCombine[A, B, C](decA: Decoder[A], decB: Decoder[B])(buffer: BitVector)(f: (A, B) => C): String \/ (BitVector, C) = {
    // Note: this could be written using DecodingContext but this function is called *a lot* and needs to be very fast
    decA.decode(buffer) match {
      case e @ -\/(_) => e
      case \/-((postA, a)) =>
        decB.decode(postA) match {
          case e @ -\/(_) => e
          case \/-((rest, b)) => \/-((rest, f(a, b)))
        }
      }
  }

  /**
   * Repeatedly decodes values of type `A` from the specified vector, converts each value to a `B` and appends it to an accumulator of type `B` using the `Monoid[B]`.
   * Terminates when no more bits are available in the vector. Exits upon the first error from decoding.
   *
   * @return tuple consisting of the terminating error if any and the accumulated value
   */
  final def decodeAll[A: Decoder, B: Monoid](buffer: BitVector)(f: A => B): (Option[String], B) = {
    val decoder = Decoder[A]
    var remaining = buffer
    var acc = Monoid[B].zero
    while (remaining.nonEmpty) {
      decoder.decode(remaining).fold(
        { err => return (Some(err), acc) },
        { case (newRemaining, a) =>
            remaining = newRemaining
            acc = Monoid[B].append(acc, f(a))
        }
      )
    }
    (None, acc)
  }

  /**
   * Repeatedly decodes values of type `A` from the specified vector and returns a collection of the specified type.
   * Terminates when no more bits are available in the vector. Exits upon the first error from decoding.
   */
  final def decodeCollect[F[_], A](dec: Decoder[A])(buffer: BitVector)(implicit cbf: collection.generic.CanBuildFrom[F[A], A, F[A]]): String \/ F[A] = {
    val bldr = cbf()
    var remaining = buffer
    var error: Option[String] = None
    while (remaining.nonEmpty) {
      dec.decode(remaining) match {
        case \/-((rest, value)) =>
          bldr += value
          remaining = rest
        case -\/(err) =>
          error = Some(err)
          remaining = BitVector.empty
      }
    }
    error.toLeftDisjunction(bldr.result)
  }
}

/** Companion for [[Decoder]]. */
object Decoder extends DecoderFunctions {

  /** Provides syntax for summoning a `Decoder[A]` from implicit scope. */
  def apply[A](implicit dec: Decoder[A]): Decoder[A] = dec

  /** Creates a decoder that always decodes the specified value and returns the input bit vector unmodified. */
  def point[A](a: => A): Decoder[A] = new Decoder[A] {
    private lazy val value = a
    def decode(bits: BitVector) = \/.right((bits, value))
    override def toString = s"const($value)"
  }

  implicit val monadInstance: Monad[Decoder] = new Monad[Decoder] {
    def point[A](a: => A) = Decoder.point(a)
    def bind[A, B](decoder: Decoder[A])(f: A => Decoder[B]) = decoder.flatMap(f)
  }

  implicit def monoidInstance[A: Monoid]: Monoid[Decoder[A]] = new Monoid[Decoder[A]] {
    def zero = Decoder.point(Monoid[A].zero)
    def append(x: Decoder[A], y: => Decoder[A]) = new Decoder[A] {
      private lazy val yy = y
      def decode(bits: BitVector) = for {
        first <- x.decode(bits)
        second <- yy.decode(first._1)
      } yield (second._1, Monoid[A].append(first._2, second._2))
    }
  }
}
