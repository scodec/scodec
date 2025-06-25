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

import scala.compiletime.*

/** The tuple which is the result of removing all 'Unit' types from the tuple 'A'. */
type DropUnits[A <: Tuple] <: Tuple = A match
  case hd *: tl =>
    hd match
      case Unit => DropUnits[tl]
      case Any  => hd *: DropUnits[tl]
  case EmptyTuple => EmptyTuple

object DropUnits:
  // actually it should be InlineFoldR.Step[[T <: Tuple] =>> T => DropUnits[T]], but it leads to endless comipilation for scala 3.3.6 (https://github.com/scala/scala3/issues/23110)
  object DropStep extends InlineFoldR.Step[[T <: Tuple] =>> T => Tuple]:
    inline def apply[Elem, T <: Tuple](
        acc: T => Tuple
    ): (Elem *: T) => Tuple =
      (elem: Elem *: T) =>
        inline erasedValue[Elem & Matchable] match
          case _: Unit => acc(elem.tail)
          case _       => elem.head *: acc(elem.tail)

  inline def drop[A <: Tuple](a: A): DropUnits[A] =
    InlineFoldR
      .fold[[T <: Tuple] =>> T => Tuple, A](
        (_: EmptyTuple) => EmptyTuple,
        DropStep
      )(a)
      .asInstanceOf[DropUnits[A]]

  object InsertStep extends InlineFoldR.Step[[T <: Tuple] =>> DropUnits[T] => T]:
    inline def apply[Elem, T <: Tuple](
        acc: DropUnits[T] => T
    ): DropUnits[Elem *: T] => Elem *: T = dropped =>
      inline erasedValue[Elem & Matchable] match
        case _: Unit => (().asInstanceOf[Elem]) *: acc(dropped.asInstanceOf[DropUnits[T]])
        case _       =>
          val droppedCast = dropped.asInstanceOf[Elem *: DropUnits[T]]
          droppedCast.head *: acc(droppedCast.tail)

  inline def insert[A <: Tuple](t: DropUnits[A]): A =
    InlineFoldR
      .fold[[T <: Tuple] =>> DropUnits[T] => T, A](
        (_: EmptyTuple) => EmptyTuple,
        InsertStep
      )(t)
