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

import scala.deriving.Mirror

@annotation.implicitNotFound("""Could not prove ${A} is isomorphic to ${B}.""")
trait Iso[A, B] { self =>
  def to(a: A): B
  def from(b: B): A

  final def inverse: Iso[B, A] = new Iso[B, A] {
    def to(b: B) = self.from(b)
    def from(a: A) = self.to(a)
  }
}

private trait IsoLowPriority {
  def instance[A, B](t: A => B)(f: B => A): Iso[A, B] =
    new Iso[A, B] {
      def to(a: A) = t(a)
      def from(b: B) = f(b)
    }

  given inverse[A, B](using iso: Iso[A, B]) as Iso[B, A] = iso.inverse

  inline given productWithUnits[A <: Tuple, B](using 
    m: Mirror.ProductOf[B],
    ev: m.MirroredElemTypes =:= DropUnits[A]
  ) as Iso[A, B] = 
    instance((a: A) => fromTuple(DropUnits.drop(a)))(b => DropUnits.insert(toTuple(b)))

  protected def toTuple[A, B <: Tuple](a: A)(using m: Mirror.ProductOf[A], ev: m.MirroredElemTypes =:= B): B =
    Tuple.fromProduct(a.asInstanceOf[Product]).asInstanceOf[B]
  
  protected def fromTuple[A, B <: Tuple](b: B)(using m: Mirror.ProductOf[A], ev: m.MirroredElemTypes =:= B): A =
    m.fromProduct(b.asInstanceOf[Product]).asInstanceOf[A]
}

/** Companion for [[Iso]]. */
object Iso extends IsoLowPriority {

  /** Identity iso. */
  given id[A] as Iso[A, A] = instance[A, A](identity)(identity)

  given product[A <: Tuple, B](using m: Mirror.ProductOf[B], ev: m.MirroredElemTypes =:= A) as Iso[A, B] =
    instance[A, B](fromTuple)(toTuple)

  given singleton[A, B](using m: Mirror.ProductOf[B], ev: m.MirroredElemTypes =:= A *: EmptyTuple) as Iso[A, B] =
    instance[A, B](a => fromTuple(a *: EmptyTuple))(b => toTuple(b).head)
}
