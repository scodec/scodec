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

/**
  * Describes an error.
  *
  * An error has a message and a list of context identifiers that provide insight into where an error occurs in a large structure.
  *
  * This type is not sealed so that codecs can return domain specific
  * subtypes and dispatch on those subtypes.
  */
trait Err:

  /** Gets a description of the error. */
  def message: String

  /**
    * Gets a stack of context identifiers.
    *
    * The head of the list is the outermost (i.e., least specific) identifier.
    */
  def context: List[String]

  /** Gets a description of the error with the context identifiers prefixing the message. */
  def messageWithContext: String =
    (if context.isEmpty then "" else context.mkString("", "/", ": ")) + message

  /** Returns a new error with the specified context identifier pushed in to the context stack. */
  def pushContext(ctx: String): Err

  override def toString = messageWithContext

/** Companion for [[Err]]. */
object Err:

  case class General(message: String, context: List[String]) extends Err:
    def this(message: String) = this(message, Nil)
    def pushContext(ctx: String) = copy(context = ctx :: context)

  case class InsufficientBits(needed: Long, have: Long, context: List[String]) extends Err:
    def this(needed: Long, have: Long) = this(needed, have, Nil)
    def message = s"cannot acquire $needed bits from a vector that contains $have bits"
    def pushContext(ctx: String) = copy(context = ctx :: context)

  case class MatchingDiscriminatorNotFound[A](a: A, context: List[String]) extends Err:
    def this(a: A) = this(a, Nil)
    def message = s"could not find matching case for $a"
    def pushContext(ctx: String) = copy(context = ctx :: context)

  case class Composite(errs: List[Err], context: List[String]) extends Err:
    def this(errs: List[Err]) = this(errs, Nil)
    def message: String = errs.map(_.message).mkString("composite errors (", ",", ")")
    def push(err: Err) = copy(errs = err :: errs)
    def pushContext(ctx: String): Err = copy(context = ctx :: context)

  def apply(message: String): Err = new General(message)

  def apply(errs: List[Err]): Err = new Composite(errs)

  def fromThrowable(t: Throwable): Err =
    val m = t.getMessage
    new General(if m == null then t.getClass.getSimpleName.nn else m)

  def insufficientBits(needed: Long, have: Long): Err = new InsufficientBits(needed, have)
