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
type DropUnits[A <: Tuple] <: Tuple = A match {
  case hd *: tl => hd match {
    case Unit => DropUnits[tl]
    case _ => hd *: DropUnits[tl]
  }
  case EmptyTuple => EmptyTuple
}

object DropUnits {

  inline def drop[A <: Tuple](a: A): DropUnits[A] = {
    // Ideally, the following would work:
    // inline a match {
    //   case (_ *: t): (Unit *: tl) => drop[tl](t)
    //   case (h *: t): (hd *: tl) => h *: drop[tl](t)
    //   case EmptyTuple => EmptyTuple
    // }
    inline erasedValue[A] match {
      case _: (Unit *: tl) => drop[tl](a.asInstanceOf[Unit *: tl].tail)
      case _: (hd *: tl) => 
        val at = a.asInstanceOf[hd *: tl]
        at.head *: drop[tl](at.tail)
      case EmptyTuple => EmptyTuple
    }
  }.asInstanceOf[DropUnits[A]]

  inline def insert[A <: Tuple](t: DropUnits[A]): A = {
    inline erasedValue[A] match {
      case _: (Unit *: tl) => (()) *: (insert[tl](t.asInstanceOf[DropUnits[tl]]))
      case _: (hd *: tl) =>
        val t2 = t.asInstanceOf[NonEmptyTuple]
        t2.head.asInstanceOf[hd] *: insert[tl](t2.tail.asInstanceOf[DropUnits[tl]])
      case EmptyTuple => EmptyTuple
    }
  }.asInstanceOf[A]
}
