package scodec

import scodec.bits.BitVector

/**
 * Result of an encoding operation. This type is a right-biased `Either[Err, BitVector]`.
 *
 * @groupname Ungrouped Members
 * @groupprio 1
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 0
 */
sealed abstract class EncodeResult {

  /** Concatenates the suppled result to this result when both are successful. Otherwise, the first error is returned. */
  def ++(other: EncodeResult): EncodeResult

  /** Maps the supplied function over the successful value, if present. */
  def map(f: BitVector => BitVector): EncodeResult

  /** Maps the supplied function over the failure error, if present. */
  def mapErr(f: Err => Err): EncodeResult

  /** Maps the supplied function over the successful value, if present. */
  def flatMap(f: BitVector => EncodeResult): EncodeResult

  /** Transforms this to a value of type `B` using the supplied functions. */
  def fold[B](ifSuccessful: BitVector => B, ifFailure: Err => B): B

  /** Returns the successful value if present; otherwise throws an `IllegalArgumentException`. */
  def require: BitVector

  /** True if the attempt was successful. */
  def isSuccessful: Boolean

  /** True if the attempt was not successful. */
  def isFailure: Boolean = !isSuccessful

  /** Converts to an attempt. */
  def toAttempt: Attempt[BitVector]

  /** Converts to an option, discarding the error value. */
  def toOption: Option[BitVector]

  /** Converts to an either. */
  def toEither: Either[Err, BitVector]
}

/** Companion for [[EncodeResult]]. */
object EncodeResult {

  /** Creates a successful result. */
  def successful(bits: BitVector): EncodeResult = Successful(bits)

  /** Creates an unsuccessful result. */
  def failure(cause: Err): EncodeResult = Failure(cause)

  /** Creates a result from the supplied option. The `ifNone` value is used as the error message if `opt` is `None`. */
  def fromOption(opt: Option[BitVector], ifNone: => Err): EncodeResult =
    opt.fold(failure(ifNone))(successful)

  /** Creates a result from the supplied attempt. */
  def fromAttempt(att: Attempt[BitVector]): EncodeResult =
    att.fold(successful, failure)

  /** Creates a result from the supplied either. */
  def fromEither(e: Either[Err, BitVector]): EncodeResult =
    e.fold(failure, successful)

  /** Successful encode, resulting in the supplied bits. */
  case class Successful(bits: BitVector) extends EncodeResult {
    def ++(other: EncodeResult): EncodeResult = other match {
      case Successful(otherBits) => Successful(bits ++ otherBits)
      case f: Failure => f
    }
    def map(f: BitVector => BitVector): EncodeResult = Successful(f(bits))
    def mapErr(f: Err => Err): EncodeResult = this
    def flatMap(f: BitVector => EncodeResult): EncodeResult = f(bits)
    def fold[B](ifSuccessful: BitVector => B, ifFailure: Err => B): B = ifSuccessful(bits)
    def require: BitVector = bits
    def isSuccessful: Boolean = true
    def toAttempt: Attempt.Successful[BitVector] = Attempt.Successful(bits)
    def toOption: Some[BitVector] = Some(bits)
    def toEither: Right[Err, BitVector] = Right(bits)
  }

  /** Unsuccessful encoding due to the supplied cause. */
  case class Failure(cause: Err) extends EncodeResult {
    def ++(other: EncodeResult): EncodeResult = this
    def map(f: BitVector => BitVector): EncodeResult = this
    def mapErr(f: Err => Err): EncodeResult = Failure(f(cause))
    def flatMap(f: BitVector => EncodeResult): EncodeResult = this
    def fold[B](ifSuccessful: BitVector => B, ifFailure: Err => B): B = ifFailure(cause)
    def require: BitVector = throw new IllegalArgumentException(cause.messageWithContext)
    def isSuccessful: Boolean = false
    def toAttempt: Attempt.Failure = Attempt.Failure(cause)
    def toOption: None.type = None
    def toEither: Left[Err, BitVector] = Left(cause)
  }
}
