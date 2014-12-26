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
import shapeless.ops.coproduct._

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
}
