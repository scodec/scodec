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

import scodec.bits.{BitVector, ByteOrdering}

private[codecs] final class ShortCodec(bits: Int, signed: Boolean, ordering: ByteOrdering)
    extends Codec[Short]:

  require(
    bits > 0 && bits <= (if signed then 16 else 15),
    "bits must be in range [1, 16] for signed and [1, 15] for unsigned"
  )

  val MaxValue = ((1 << (if signed then bits - 1 else bits)) - 1).toShort
  val MinValue = (if signed then -(1 << (bits - 1)) else 0).toShort

  private val bitsL = bits.toLong

  private def description = s"$bits-bit ${if signed then "signed" else "unsigned"} short"

  override def sizeBound = SizeBound.exact(bits.toLong)

  override def encode(s: Short) =
    if s > MaxValue then
      Attempt.failure(Err(s"$s is greater than maximum value $MaxValue for $description"))
    else if s < MinValue then
      Attempt.failure(Err(s"$s is less than minimum value $MinValue for $description"))
    else Attempt.successful(BitVector.fromShort(s, bits, ordering))

  override def decode(buffer: BitVector) =
    if buffer.sizeGreaterThanOrEqual(bitsL) then
      Attempt.successful(
        DecodeResult(buffer.take(bitsL).toShort(signed, ordering), buffer.drop(bitsL))
      )
    else Attempt.failure(Err.insufficientBits(bitsL, buffer.size))

  override def toString = description
