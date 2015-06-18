package scodec

import scodec.bits.BitVector

/**
 * Provides the ability to sequence decoding operations such that the remainder of an operation is fed in to the input
 * of the next operation. This is useful when using codecs in for comprehensions for decoding purposes.
 *
 * Note: this is a domain specific fail fast state monad.
 */
sealed abstract class DecodingContext[A] { self =>

  /** Runs the sequenced decoding operations by decoding the specified bit vector. */
  def decode(buffer: BitVector): Attempt[DecodeResult[A]]

  /** Transforms the decoded value using the supplied function. */
  final def map[B](f: A => B): DecodingContext[B] = new DecodingContext[B] {
    def decode(buffer: BitVector) = self.decode(buffer).map { _ map f }
  }

  /** Transforms the decoding error using the supplied function. */
  final def mapErr(f: Err => Err): DecodingContext[A] = new DecodingContext[A] {
    def decode(buffer: BitVector) = self.decode(buffer).mapErr(f)
  }

  /**
   * Returns a context that first decodes using this context, then uses the decoded value
   * and the supplied function to generate a subsequent context, and decodes the remainder
   * of the first decoding operation with the subsequent context.
   */
  def flatMap[B](f: A => DecodingContext[B]): DecodingContext[B] = new DecodingContext[B] {
    def decode(buffer: BitVector) =
      self.decode(buffer).flatMap { result =>
        f(result.value).decode(result.remainder)
      }
  }

  /** Converts this context to a decoder instance. */
  def toDecoder: Decoder[A] = new Decoder[A] {
    def decode(buffer: BitVector) = self.decode(buffer)
  }
}

/** Provides constructors for `DecodingContext`. */
object DecodingContext {

  /** Lifts a decoder to a decoding context. */
  def apply[A](decoder: Decoder[A]): DecodingContext[A] = fromFunction(decoder.decode _)

  /** Lifts a function of the shape `BitVector => Attempt[DecodeResult[A]]` to a decoding context. */
  def fromFunction[A](f: BitVector => Attempt[DecodeResult[A]]): DecodingContext[A] = new DecodingContext[A] {
    def decode(buffer: BitVector) = f(buffer)
  }

  /** Context that gets the current buffer. */
  def get: DecodingContext[BitVector] = fromFunction(b => Attempt.successful(DecodeResult(b, b)))

  /** Context that sets the current buffer to the supplied value, ignoring the current buffer. */
  def set(buffer: BitVector): DecodingContext[Unit] = fromFunction(_ => Attempt.successful(DecodeResult((), buffer)))

  /** Context that sets the current buffer to the the result of applying the supplied function to the current buffer. */
  def modify(f: BitVector => BitVector): DecodingContext[Unit] = fromFunction(b => Attempt.successful(DecodeResult((), f(b))))

  /** Lifts a value of `Attempt[A]` in to a decoding context. */
  def liftAttempt[A](res: Attempt[A]): DecodingContext[A] =
    fromFunction { bv => res.fold(err => Attempt.failure(err), a => Attempt.successful(DecodeResult(a, bv))) }
}

