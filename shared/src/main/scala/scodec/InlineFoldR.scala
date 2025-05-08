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

object InlineFoldR:
  trait Step[Acc[_ <: Tuple]]:
    inline def apply[Elem, T <: Tuple](acc: Acc[T]): Acc[Elem *: T]

  /** Inline version of foldRight that works on tuples */
  inline def fold[Acc[_ <: Tuple], T <: Tuple](
      acc: Acc[EmptyTuple],
      f: Step[Acc]
  ): Acc[T] =
    inline erasedValue[T] match
      case _: EmptyTuple => acc.asInstanceOf[Acc[T]]
      case _: (h1 *: h2 *: h3 *: h4 *: h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *:
            h14 *: h15 *: h16 *: ts) =>
        f[
          h1,
          h2 *: h3 *: h4 *: h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *:
            h16 *: ts
        ](
          f[
            h2,
            h3 *: h4 *: h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *:
              h16 *: ts
          ](
            f[
              h3,
              h4 *: h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *:
                ts
            ](
              f[
                h4,
                h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts
              ](
                f[
                  h5,
                  h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts
                ](
                  f[
                    h6,
                    h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts
                  ](
                    f[
                      h7,
                      h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts
                    ](
                      f[
                        h8,
                        h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts
                      ](
                        f[
                          h9,
                          h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts
                        ](
                          f[h10, h11 *: h12 *: h13 *: h14 *: h15 *: h16 *: ts](
                            f[h11, h12 *: h13 *: h14 *: h15 *: h16 *: ts](
                              f[h12, h13 *: h14 *: h15 *: h16 *: ts](
                                f[h13, h14 *: h15 *: h16 *: ts](
                                  f[h14, h15 *: h16 *: ts](
                                    f[h15, h16 *: ts](
                                      f[h16, ts](fold[Acc, ts](acc, f))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
          .asInstanceOf[Acc[T]]
      case _: (h1 *: h2 *: h3 *: h4 *: ts) =>
        f[h1, h2 *: h3 *: h4 *: ts](
          f[h2, h3 *: h4 *: ts](
            f[h3, h4 *: ts](f[h4, ts](fold[Acc, ts](acc, f)))
          )
        ).asInstanceOf[Acc[T]]
      case _: (h *: ts) =>
        f[h, ts](fold[Acc, ts](acc, f)).asInstanceOf[Acc[T]]
