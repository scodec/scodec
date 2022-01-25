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

private[codecs] final class ConstrainedVariableSizeCodec[A](
    sizeCodec: Codec[Long],
    valueCodec: Codec[A],
    minSize: Long,
    maxSize: Long
) extends Codec[A]:
  require(minSize < maxSize)
  require(minSize > -1)

  val minSizeBits = minSize * 8
  val maxSizeBits = maxSize * 8

  private def checkBoundaries(sz: Long) = minSizeBits <= sz && sz <= maxSizeBits

  private val decoder = sizeCodec.flatMap { sz =>
    if checkBoundaries(sz) then codecs.fixedSizeBits(sz, valueCodec).complete
    else codecs.fail[A](Err(s"Size out of bounds: $minSizeBits <= $sz <= $maxSizeBits is not true"))
  }

  def sizeBound = sizeCodec.sizeBound.atLeast

  override def encode(a: A) = valueCodec.complete.encode(a).flatMap { enc =>
    val sz = enc.size

    if checkBoundaries(sz) then
      sizeCodec.encode(sz).map(_ ++ enc).mapErr(e => failMsg(a, e.messageWithContext))
    else
      Attempt.failure(Err(s"Size out of bounds: $minSizeBits <= $sz <= $maxSizeBits is not true"))

  }

  private def failMsg(a: A, msg: String): Err =
    Err.General(s"failed to encode size of [$a]: $msg", List("size"))

  override def decode(buffer: BitVector) =
    decoder.decode(buffer)

  override def toString = s"constrainedVariableSizeBits($sizeCodec, $valueCodec)"
