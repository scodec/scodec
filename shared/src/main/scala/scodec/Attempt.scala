/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec

import scala.util.Try
import scala.util.control.NonFatal

/**
  * Right biased `Either[Err, A]`.
  *
  * An `Attempt` is either an `Attempt.Successful` or an `Attempt.Failure`. Attempts can be created
  * by calling `Attempt.successful` or `Attempt.failure`, as well as converting from an `Option` via
  * `fromOption`.
  */
sealed abstract class Attempt[+A] extends Product, Serializable:

  /** Maps the supplied function over the successful value, if present. */
  def map[B](f: A => B): Attempt[B]

  /** Maps the supplied function over the failure error, if present. */
  def mapErr(f: Err => Err): Attempt[A]

  /** Maps the supplied function over the successful value, if present. */
  def flatMap[B](f: A => Attempt[B]): Attempt[B]

  /** Converts an `Attempt[Attempt[X]]` in to an `Attempt[X]`. */
  def flatten[B](using A <:< Attempt[B]): Attempt[B]

  /** Transforms this attempt to a value of type `B` using the supplied functions. */
  def fold[B](ifFailure: Err => B, ifSuccessful: A => B): B

  /** Returns the sucessful value if successful, otherwise the supplied default. */
  def getOrElse[B >: A](default: => B): B

  /** Returns this attempt if successful, otherwise the fallback attempt. */
  def orElse[B >: A](fallback: => Attempt[B]): Attempt[B]

  /**
    * If this attempt is a failure, and the supplied partial function is defined for the cause of the failure,
    * a successful attempt is returned. If this attempt is successful or the supplied function is not defined
    * for the cause of the failure, this attempt is returned unmodified.
    */
  def recover[B >: A](f: PartialFunction[Err, B]): Attempt[B]

  /**
    * If this attempt is a failure, and the supplied partial function is defined for the cause of the failure,
    * the result of applying that function is returned. If this attempt is successful or the supplied
    * function is not defined for the cause of the failure, this attempt is returned unmodified.
    */
  def recoverWith[B >: A](f: PartialFunction[Err, Attempt[B]]): Attempt[B]

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

  /** Converts to a try. */
  def toTry: Try[A]

/** Companion for [[Attempt]]. */
object Attempt:

  /** Creates a successful attempt. */
  def successful[A](a: A): Attempt[A] = Successful(a)

  /** Creates an unsuccessful attempt. */
  def failure[A](err: Err): Attempt[A] = Failure(err)

  /** Creates a successful attempt if the condition succeeds otherwise create a unsuccessful attempt. */
  def guard(condition: => Boolean, err: String): Attempt[Unit] =
    if condition then successful(()) else failure(Err(err))

  /** Creates a successful attempt if the condition succeeds otherwise create a unsuccessful attempt. */
  def guard(condition: => Boolean, err: => Err): Attempt[Unit] =
    if condition then successful(()) else failure(err)

  /** Creates a attempt from a try. */
  def fromTry[A](t: Try[A]): Attempt[A] = t.fold(e => failure(Err.fromThrowable(e)), successful)

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
  case class Successful[A](value: A) extends Attempt[A]:
    def map[B](f: A => B): Attempt[B] = Successful(f(value))
    def mapErr(f: Err => Err): Attempt[A] = this
    def flatMap[B](f: A => Attempt[B]): Attempt[B] = f(value)
    def flatten[B](using ev: A <:< Attempt[B]): Attempt[B] = ev(value)
    def fold[B](ifFailure: Err => B, ifSuccessful: A => B): B = ifSuccessful(value)
    def getOrElse[B >: A](default: => B): B = value
    def orElse[B >: A](fallback: => Attempt[B]) = this
    def recover[B >: A](f: PartialFunction[Err, B]): Attempt[B] = this
    def recoverWith[B >: A](f: PartialFunction[Err, Attempt[B]]): Attempt[B] = this
    def require: A = value
    def isSuccessful: Boolean = true
    def toOption: Some[A] = Some(value)
    def toEither: Right[Err, A] = Right(value)
    def toTry: Try[A] = scala.util.Success(value)

  /** Failed attempt. */
  case class Failure(cause: Err) extends Attempt[Nothing]:
    def map[B](f: Nothing => B): Attempt[B] = this
    def mapErr(f: Err => Err): Attempt[Nothing] = Failure(f(cause))
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
    def flatten[B](using ev: Nothing <:< Attempt[B]): Attempt[B] = this
    def fold[B](ifFailure: Err => B, ifSuccessful: Nothing => B): B = ifFailure(cause)
    def getOrElse[B >: Nothing](default: => B): B = default
    def orElse[B >: Nothing](fallback: => Attempt[B]) = fallback
    def recover[B >: Nothing](f: PartialFunction[Err, B]): Attempt[B] =
      if f.isDefinedAt(cause) then Attempt.successful(f(cause))
      else this
    def recoverWith[B >: Nothing](f: PartialFunction[Err, Attempt[B]]): Attempt[B] =
      if f.isDefinedAt(cause) then f(cause)
      else this
    def require: Nothing = throw new IllegalArgumentException(cause.messageWithContext)
    def isSuccessful: Boolean = false
    def toOption: None.type = None
    def toEither: Left[Err, Nothing] = Left(cause)
    def toTry: Try[Nothing] =
      scala.util.Failure(new Exception(s"Error occurred: ${cause.messageWithContext}"))
