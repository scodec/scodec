package scodec

/**
 * Right biased `Either[Err, A]`.
 *
 * An `Attempt` is either an `Attempt.Successful` or an `Attempt.Failure`. Attempts can be created
 * by calling `Attempt.successful` or `Attempt.failure`, as well as converting from an `Option` via
 * `fromOption`.
 *
 * @groupname Ungrouped Members
 * @groupprio 1
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 0
 */
sealed abstract class Attempt[+A] {

  /** Maps the supplied function over the successful value, if present. */
  def map[B](f: A => B): Attempt[B]

  /** Maps the supplied function over the failure error, if present. */
  def mapErr(f: Err => Err): Attempt[A]

  /** Maps the supplied function over the successful value, if present. */
  def flatMap[B](f: A => Attempt[B]): Attempt[B]

  /** Transforms this attempt to a value of type `B` using the supplied functions. */
  def fold[B](ifFailure: Err => B, ifSuccessful: A => B): B

  /** Returns the successful value if present; otherwise throws an `IllegalArgumentException`. */
  def require: A

  /** True if attempt was successful. */
  def isSuccessful: Boolean

  /** True if attempt was not successful. */
  def isFailure: Boolean = !isSuccessful

  /** Converts to an option, discarding the error value. */
  def toOption: Option[A]

  /** Converts to an either. */
  def toEither: Either[Err, A]
}

/** Companion for [[Attempt]]. */
object Attempt {

  /** Creates a successful attempt. */
  def successful[A](a: A): Attempt[A] = Successful(a)

  /** Creates an unsuccessful attempt. */
  def failure[A](err: Err): Attempt[A] = Failure(err)

  /** Creates an attempt from the supplied option. The `ifNone` value is used as the error message if `opt` is `None`. */
  def fromOption[A](opt: Option[A], ifNone: => Err): Attempt[A] =
    opt.fold(failure[A](ifNone))(successful)

  /** Creates an attempt from the supplied option. The `ifNone` value is used as the success value if `opt` is `None`. */
  def fromErrOption[A](opt: Option[Err], ifNone: => A): Attempt[A] =
    opt.fold(successful(ifNone))(failure)

  /** Creates an attempt from the supplied either. */
  def fromEither[A](e: Either[Err, A]): Attempt[A] =
    e.fold(failure, successful)

  /** Successful attempt. */
  case class Successful[A](value: A) extends Attempt[A] {
    def map[B](f: A => B): Attempt[B] = Successful(f(value))
    def mapErr(f: Err => Err): Attempt[A] = this
    def flatMap[B](f: A => Attempt[B]): Attempt[B] = f(value)
    def fold[B](ifFailure: Err => B, ifSuccessful: A => B): B = ifSuccessful(value)
    def require: A = value
    def isSuccessful: Boolean = true
    def toOption: Some[A] = Some(value)
    def toEither: Right[Err, A] = Right(value)
  }

  /** Failed attempt. */
  case class Failure(cause: Err) extends Attempt[Nothing] {
    def map[B](f: Nothing => B): Attempt[B] = this
    def mapErr(f: Err => Err): Attempt[Nothing] = Failure(f(cause))
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
    def fold[B](ifFailure: Err => B, ifSuccessful: Nothing => B): B = ifFailure(cause)
    def require: Nothing = throw new IllegalArgumentException(cause.messageWithContext)
    def isSuccessful: Boolean = false
    def toOption: None.type = None
    def toEither: Left[Err, Nothing] = Left(cause)
  }
}
