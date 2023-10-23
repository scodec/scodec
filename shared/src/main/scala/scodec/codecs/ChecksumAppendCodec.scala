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

private[codecs] final class ChecksumAppendCodec[V](
    target: Codec[V],
    bitSize: Int,
    compute: BitVector => BitVector,
    validate: Boolean,
    framing: BitVector => BitVector
) extends Codec[V]:

  def decode(bits: BitVector): Attempt[DecodeResult[V]] =
    val (noCrcBytes, maybeCrcBytes) = bits.splitAt(bits.size - bitSize)
    target
      .decode(noCrcBytes)
      .flatMap:
        case DecodeResult(value, remainder) =>
          val valueBits = noCrcBytes.dropRight(remainder.size)
          val actualCrc = compute(framing(valueBits))
          val (expectedCrc, nextBits) = (remainder ++ maybeCrcBytes).splitAt(bitSize)
          if !validate || expectedCrc == actualCrc then
            Attempt.successful(DecodeResult(value, nextBits))
          else Attempt.failure(ChecksumMismatch(valueBits, expectedCrc, actualCrc))

  def encode(value: V): Attempt[BitVector] =
    target
      .encode(value)
      .map: bits =>
        bits ++ compute(framing(bits))

  def sizeBound: SizeBound = target.sizeBound + SizeBound.exact(bitSize)
