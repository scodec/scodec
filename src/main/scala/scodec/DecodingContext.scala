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
  def decode(buffer: BitVector): DecodeResult[A]

  /** Transforms the decoded value using the supplied function. */
  final def map[B](f: A => B): DecodingContext[B] = new DecodingContext[B] {
    def decode(buffer: BitVector): DecodeResult[B] = self.decode(buffer).map(f)
  }

  /** Transforms the decoding error using the supplied function. */
  final def mapErr(f: Err => Err): DecodingContext[A] = new DecodingContext[A] {
    def decode(buffer: BitVector): DecodeResult[A] = self.decode(buffer).mapErr(f)
  }

  /**
   * Returns a context that first decodes using this context, then uses the decoded value
   * and the supplied function to generate a subsequent context, and decodes the remainder
   * of the first decoding operation with the subsequent context.
   */
  def flatMap[B](f: A => DecodingContext[B]): DecodingContext[B] = new DecodingContext[B] {
    def decode(buffer: BitVector): DecodeResult[B] =
      self.decode(buffer).flatMapWithRemainder { (a, rem) =>
        f(a).decode(rem)
      }
  }

  /** Converts this context to a decoder instance. */
  def toDecoder: Decoder[A] = new Decoder[A] {
    def decode(buffer: BitVector): DecodeResult[A] = self.decode(buffer)
  }
}

/** Provides constructors for `DecodingContext`. */
object DecodingContext {

  /** Lifts a decoder to a decoding context. */
  def apply[A](decoder: Decoder[A]): DecodingContext[A] = fromFunction(decoder.decode _)

  /** Lifts a function of the shape `BitVector => DecodeResult[A]` to a decoding context. */
  def fromFunction[A](f: BitVector => DecodeResult[A]): DecodingContext[A] = new DecodingContext[A] {
    def decode(buffer: BitVector): DecodeResult[A] = f(buffer)
  }

  /** Context that gets the current buffer. */
  def get: DecodingContext[BitVector] = fromFunction(b => DecodeResult.successful(b, b))

  /** Context that sets the current buffer to the supplied value, ignoring the current buffer. */
  def set(buffer: BitVector): DecodingContext[Unit] = fromFunction(_ => DecodeResult.successful((), buffer))

  /** Context that sets the current buffer to the the result of applying the supplied function to the current buffer. */
  def modify(f: BitVector => BitVector): DecodingContext[Unit] = fromFunction(b => DecodeResult.successful((), f(b)))

  /** Lifts a value of `Attempt[A]` in to a decoding context. */
  def liftAttempt[A](res: Attempt[A]): DecodingContext[A] =
    fromFunction { bv => res.fold(a => DecodeResult.successful(a, bv), err => DecodeResult.failure(err)) }
}

