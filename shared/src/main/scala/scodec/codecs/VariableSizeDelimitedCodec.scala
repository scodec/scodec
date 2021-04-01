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

private[codecs] final class VariableSizeDelimitedCodec[A](
    delimiterCodec: Codec[Unit],
    valueCodec: Codec[A],
    multipleValueSize: Long = 0L
) extends Codec[A]:

  val delimiter = delimiterCodec.encode(()).require
  val segmentSize = valueCodec.sizeBound.exact.getOrElse(multipleValueSize)

  require(
    segmentSize > 0,
    "valueCodec must have an exact sizeBound or you need to specify multipleValueSize"
  )

  require(
    delimiterCodec.sizeBound.lowerBound >= segmentSize,
    "delimiterCodec cannot be smaller than the sizeBound of the valueCodec"
  )

  def sizeBound = delimiterCodec.sizeBound.atLeast

  override def encode(a: A) =
    for
      encA <- valueCodec.encode(a)
    yield encA ++ delimiter

  override def decode(buffer: BitVector) =
    val index = findDelimiterIndex(buffer)
    if index != -1 then
      val valueBuffer = buffer.take(index)
      val remainder = buffer.drop(index + delimiter.size)
      valueCodec.decode(valueBuffer).map(decodeResult => decodeResult.mapRemainder(_ ++ remainder))
    else
      Attempt.failure(Err(s"expected delimiter $delimiterCodec"))

  private def findDelimiterIndex(buffer: BitVector): Long =
    var offset = 0L
    while
      if buffer.drop(offset).startsWith(delimiter) then
        return offset
      offset += segmentSize
      offset < buffer.size
    do ()
    -1

  override def toString = s"VariableSizeDelimited($delimiterCodec, $valueCodec, $multipleValueSize)"
