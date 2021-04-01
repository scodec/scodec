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
package codecs

import scodec.bits.BitVector

private[codecs] final class ByteAlignedCodec[A](codec: Codec[A]) extends Codec[A]:

  private def padAmount(size: Long) =
    val mod = size % 8
    if mod == 0 then 0 else 8 - mod

  def sizeBound =
    val sz = codec.sizeBound
    val lb = sz.lowerBound + padAmount(sz.lowerBound)
    val ub = sz.upperBound.map(ub => ub + padAmount(ub))
    SizeBound(lb, ub)

  def encode(a: A) =
    codec.encode(a).map { enc =>
      val pad = padAmount(enc.size)
      if pad == 0 then enc
      else enc.padTo(enc.size + pad)
    }

  def decode(b: BitVector) =
    codec.decode(b).map { res =>
      val taken = b.size - res.remainder.size
      val pad = padAmount(taken)
      if pad == 0 then res
      else DecodeResult(res.value, res.remainder.drop(pad))
    }

  override def toString = s"byteAligned($codec)"
