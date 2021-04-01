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

import scala.annotation.tailrec

private[scodec] object VarPackedDecimalCodec extends Codec[Long]:
  override def sizeBound = SizeBound.bounded(1L, 9L)

  def encode(i: Long): Attempt[BitVector] =
    if i < 0 then Attempt.failure(Err("VarPackedDecimal cannot encode negative longs"))
    else runEncoding(i, Attempt.successful(BitVector.empty))

  def decode(buffer: BitVector): Attempt[DecodeResult[Long]] = runDecoding(buffer, 0L)

  @tailrec
  private def runDecoding(buffer: BitVector, value: Long): Attempt[DecodeResult[Long]] =
    if buffer.isEmpty then Attempt.successful(DecodeResult(value, buffer))
    else if buffer.sizeLessThan(4) then Attempt.failure(Err.InsufficientBits(4L, buffer.size, Nil))
    else runDecoding(buffer.drop(4), value * 10 + buffer.take(4).toInt(false))

  @tailrec
  private def runEncoding(value: Long, acc: Attempt[BitVector]): Attempt[BitVector] =
    if value < 10L then codecs.uint(4).encode(value.toInt).flatMap(x => acc.map(x ++ _))
    else runEncoding(value / 10, codecs.uint(4).encode((value % 10).toInt).flatMap(x => acc.map(x ++ _)))
