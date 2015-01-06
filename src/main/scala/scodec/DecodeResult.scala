package scodec

import scodec.bits.BitVector

/**
 * Result of a decoding operation. This type is a right-biased and value-biased
 * `Either[Err, (A, BitVector)]`, where the right value is a tuple consisting of
 * the successfully decoded value and the remaining bits -- those that were not
 * used in decoding `A`.
 *
 * In this context, value-biased refers to the fact that `map`, `flatMap`, and
 * other methods are defined to only operation on the value type `A` -- not the
 * tuple `(A, BitVector)` like a right-biased either. However, many methods
 * have sibling methods with the suffix `WithRemainder` that provides a variant
 * that allows access or manipulation of the remainder in addition to the value.
 * For instance, [[map]] and [[mapWithRemainder]].
 *
 * @groupname Ungrouped Members
 * @groupprio 1
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 0
 */
sealed abstract class DecodeResult[+A] {

  /** Maps the supplied function over the successful value, if present. */
  def map[B](f: A => B): DecodeResult[B]

  /** Maps the supplied function over the successful value and remainder, if present. */
  def mapWithRemainder[B](f: (A, BitVector) => (B, BitVector)): DecodeResult[B]

  /** Maps the supplied function over the failure error, if present. */
  def mapErr(f: Err => Err): DecodeResult[A]

  /**
   * Maps the supplied function over the successful value, if present.
   * Caution: the remainder of this result is ignored and thrown away. If this is
   * not desirable, use [[flatMapWithRemainder]] or [[DecodingContext]].
   */
  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B]

  /** Maps the supplied function over the successful value and remainder, if present. */
  def flatMapWithRemainder[B](f: (A, BitVector) => DecodeResult[B]): DecodeResult[B]

  /** Transforms this to a value of type `B` using the supplied functions. */
  def fold[B](ifSuccessful: A => B, ifFailure: Err => B): B

  /** Transforms this to a value of type `B` using the supplied functions. */
  def foldWithRemainder[B](ifSuccessful: (A, BitVector) => B, ifFailure: Err => B): B

  /** Returns the successful value if present; otherwise throws an `IllegalArgumentException`. */
  def require: A

  /** Returns the successful value and remainder if present; otherwise throws an `IllegalArgumentException`. */
  def requireWithRemainder: (A, BitVector)

  /** True if the attempt was successful. */
  def isSuccessful: Boolean

  /** True if the attempt was not successful. */
  def isFailure: Boolean = !isSuccessful

  /** Converts to an attempt. */
  def toAttempt: Attempt[A]

  /** Converts to an attempt that includes the remainder. */
  def toAttemptWithRemainder: Attempt[(A, BitVector)]

  /** Converts to an option. */
  def toOption: Option[A]

  /** Converts to an option that includes the remainder. */
  def toOptionWithRemainder: Option[(A, BitVector)]

  /** Converts to an either. */
  def toEither: Either[Err, A]

  /** Converts to an either that includes the remainder. */
  def toEitherWithRemainder: Either[Err, (A, BitVector)]
}

/** Companion for [[DecodeResult]]. */
object DecodeResult {

  /** Creates a successful result. */
  def successful[A](value: A, remainder: BitVector): DecodeResult[A] = Successful(value, remainder)

  /** Creates an unsuccessful result. */
  def failure[A](cause: Err): DecodeResult[A] = Failure(cause)

  /** Creates a result from the supplied option. The `ifNone` value is used as the error message if `opt` is `None`. */
  def fromOption[A](opt: Option[(A, BitVector)], ifNone: => Err): DecodeResult[A] =
    opt.fold(failure[A](ifNone)) { case (value, remainder) => successful(value, remainder) }

  def fromErrOption[A](opt: Option[Err], ifNone: => (A, BitVector)): DecodeResult[A] = opt match {
    case None =>
      val (value, rem) = ifNone
      successful(value, rem)
    case Some(err) => failure(err)
  }

  /** Creates a result from the supplied attempt and remainder. */
  def fromAttempt[A](att: Attempt[A], remainder: BitVector): DecodeResult[A] =
    att.fold(successful(_, remainder), failure)

  /** Creates a result from the supplied attempt. */
  def fromAttempt[A](att: Attempt[(A, BitVector)]): DecodeResult[A] =
    att.fold({ case (a, rem) => successful(a, rem) }, failure)

  /** Creates a result from the supplied either and remainder. */
  def fromEither[A](e: Either[Err, A], remainder: BitVector): DecodeResult[A] =
    e.fold(failure, successful(_, remainder))

  /** Creates a result from the supplied either. */
  def fromEither[A](e: Either[Err, (A, BitVector)]): DecodeResult[A] =
    e.fold(failure, { case (a, rem) => successful(a, rem) })


  /** Successful decode, resulting in the supplied value and remainder. */
  case class Successful[A](value: A, remainder: BitVector) extends DecodeResult[A] {
    def map[B](f: A => B): DecodeResult[B] = Successful(f(value), remainder)
    def mapWithRemainder[B](f: (A, BitVector) => (B, BitVector)): DecodeResult[B] = {
      val (b, rem) = f(value, remainder)
      Successful(b, rem)
    }
    def mapErr(f: Err => Err): DecodeResult[A] = this
    def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] = f(value)
    def flatMapWithRemainder[B](f: (A, BitVector) => DecodeResult[B]): DecodeResult[B] = f(value, remainder)
    def fold[B](ifSuccessful: A => B, ifFailure: Err => B): B = ifSuccessful(value)
    def foldWithRemainder[B](ifSuccessful: (A, BitVector) => B, ifFailure: Err => B): B = ifSuccessful(value, remainder)
    def require: A = value
    def requireWithRemainder: (A, BitVector) = (value, remainder)
    def isSuccessful: Boolean = true
    def toAttempt: Attempt.Successful[A] = Attempt.Successful(value)
    def toAttemptWithRemainder: Attempt.Successful[(A, BitVector)] = Attempt.Successful((value, remainder))
    def toOption: Some[A] = Some(value)
    def toOptionWithRemainder: Some[(A, BitVector)] = Some((value, remainder))
    def toEither: Right[Err, A] = Right(value)
    def toEitherWithRemainder: Right[Err, (A, BitVector)] = Right((value, remainder))
  }

  case class Failure(cause: Err) extends DecodeResult[Nothing] {
    def map[B](f: Nothing => B): DecodeResult[B] = this
    def mapWithRemainder[B](f: (Nothing, BitVector) => (B, BitVector)): DecodeResult[B] = this
    def mapErr(f: Err => Err): DecodeResult[Nothing] = Failure(f(cause))
    def flatMap[B](f: Nothing => DecodeResult[B]): DecodeResult[B] = this
    def flatMapWithRemainder[B](f: (Nothing, BitVector) => DecodeResult[B]): DecodeResult[B] = this
    def fold[B](ifSuccessful: Nothing => B, ifFailure: Err => B): B = ifFailure(cause)
    def foldWithRemainder[B](ifSuccessful: (Nothing, BitVector) => B, ifFailure: Err => B): B = ifFailure(cause)
    def require: Nothing = throw new IllegalArgumentException(cause.messageWithContext)
    def requireWithRemainder: (Nothing, BitVector) = throw new IllegalArgumentException(cause.messageWithContext)
    def isSuccessful: Boolean = false
    def toAttempt: Attempt.Failure = Attempt.Failure(cause)
    def toAttemptWithRemainder: Attempt.Failure = Attempt.Failure(cause)
    def toOption: None.type = None
    def toOptionWithRemainder: None.type = None
    def toEither: Left[Err, Nothing] = Left(cause)
    def toEitherWithRemainder: Left[Err, (Nothing, BitVector)] = Left(cause)
  }
}
