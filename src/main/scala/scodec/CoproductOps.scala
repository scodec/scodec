/*
 * Copyright (c) 2014 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scodec

import shapeless._

/** Operations on coproducts from Shapeless 2.1 backported to 2.0. */
object CoproductOps {

  sealed trait Align[A <: Coproduct, B <: Coproduct] {
    def apply(a: A): B
  }

  object Align {
    implicit def cnilAlign: Align[CNil, CNil] = new Align[CNil, CNil] {
      def apply(cn: CNil) = cn
    }

    implicit def coproductAlign[A <: Coproduct, BH, BT <: Coproduct, R <: Coproduct](implicit
      remove: Remove.Aux[A, BH, R], alignTail: Align[R, BT]
    ): Align[A, BH :+: BT] = new Align[A, BH :+: BT] {
      def apply(a: A) = remove(a) match {
        case Left(bh) => Inl(bh)
        case Right(rest) => Inr(alignTail(rest))
      }
    }
  }

  implicit class AlignSyntax[C <: Coproduct](val self: C) extends AnyVal {
    def align[To <: Coproduct](implicit align: Align[C, To]): To = align(self)
  }


  trait Remove[C <: Coproduct, U] extends DepFn1[C] {
    type Rest <: Coproduct
    type Out = Either[U, Rest]
    def inverse(r: Either[U, Rest]): C

    def coproduct(c: C): U :+: Rest = apply(c) match {
      case Left(u)  => Inl(u)
      case Right(r) => Inr(r)
    }
  }

  trait LowPriorityRemove {
    type Aux[C <: Coproduct, U, Rest0 <: Coproduct] = Remove[C, U] { type Rest = Rest0 }

    // Must be given a lower priority than removeHead, so that:
    // - the two don't collide for coproducts with repeated types
    // - the first element of type I in C is removed
    implicit def removeTail[H, T <: Coproduct, U](implicit
      tailRemove: Remove[T, U]
    ): Aux[H :+: T, U, H :+: tailRemove.Rest] = new Remove[H :+: T, U] {
      type Rest = H :+: tailRemove.Rest

      def apply(c: H :+: T) = c match {
        case Inl(h) => Right(Inl(h))
        case Inr(t) => tailRemove(t) match {
          case Left(i)  => Left(i)
          case Right(r) => Right(Inr(r))
        }
      }

      def inverse(r: Either[U, H :+: tailRemove.Rest]) = r match {
        case Left(i)       => Inr(tailRemove.inverse(Left(i)))
        case Right(Inl(h)) => Inl(h)
        case Right(Inr(t)) => Inr(tailRemove.inverse(Right(t)))
      }
    }
  }

  object Remove extends LowPriorityRemove {
    def apply[C <: Coproduct, U](implicit remove: Remove[C, U]): Aux[C, U, remove.Rest] = remove

    implicit def removeHead[H, T <: Coproduct]: Aux[H :+: T, H, T] = new Remove[H :+: T, H] {
      type Rest = T

      def apply(c: H :+: T) = c match {
        case Inl(h) => Left(h)
        case Inr(t) => Right(t)
      }

      def inverse(r: Either[H, T]) = r match {
        case Left(h)  => Inl(h)
        case Right(t) => Inr(t)
      }
    }
  }

}
