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
package internal

import scodec.bits.BitVector

private[scodec] final class VariableSizeCodec[A](
    sizeCodec: Codec[Long],
    valueCodec: Codec[A],
    sizePadding: Long
) extends Codec[A]:

  private val decoder = sizeCodec.flatMap(sz => codecs.fixedSizeBits(sz - sizePadding, valueCodec))

  def sizeBound = sizeCodec.sizeBound.atLeast

  override def encode(a: A) =
    for
      encA <- valueCodec.encode(a)
      encSize <- sizeCodec.encode(encA.size + sizePadding).mapErr { e =>
        fail(a, e.messageWithContext)
      }
    yield encSize ++ encA

  private def fail(a: A, msg: String): Err =
    Err.General(s"failed to encode size of [$a]: $msg", List("size"))

  override def decode(buffer: BitVector) =
    decoder.decode(buffer)

  override def toString = s"variableSizeBits($sizeCodec, $valueCodec)"
