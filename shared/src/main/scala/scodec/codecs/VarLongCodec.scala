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

import java.nio.{ByteBuffer, ByteOrder}

import scodec.bits.ByteOrdering.BigEndian
import scodec.bits.{BitVector, ByteOrdering}

import scala.annotation.tailrec

private[codecs] final class VarLongCodec(ordering: ByteOrdering) extends Codec[Long]:
  import VarLongCodec.*

  override val sizeBound: SizeBound = SizeBound.bounded(8L, 80L)

  override def encode(i: Long): Attempt[BitVector] =
    val buffer = ByteBuffer.allocate(10).order(ByteOrder.BIG_ENDIAN).nn
    val encoder = if ordering == BigEndian then BEEncoder else LEEncoder
    val written = encoder(i, buffer)
    buffer.flip()
    val relevantBits = BitVector.view(buffer).take(written.toLong)
    val bits = if ordering == BigEndian then relevantBits else relevantBits.reverseByteOrder
    Attempt.successful(bits)

  override def decode(buffer: BitVector): Attempt[DecodeResult[Long]] =
    val decoder = if ordering == BigEndian then BEDecoder else LEDecoder
    decoder(buffer)

  override def toString = "variable-length long"

  private val BEEncoder = (value: Long, buffer: ByteBuffer) => runEncodingBE(value, buffer, 8)

  private val LEEncoder = (value: Long, buffer: ByteBuffer) => runEncodingLE(value, buffer, 8, 0)

  @tailrec
  private def runEncodingBE(value: Long, buffer: ByteBuffer, size: Int): Int =
    if (value & MoreBytesMask) != 0 then
      buffer.put((value & RelevantDataBits | MostSignificantBit).toByte)
      runEncodingBE(value >>> BitsPerByte, buffer, size + 8)
    else
      buffer.put(value.toByte)
      size

  @tailrec
  private def runEncodingLE(value: Long, buffer: ByteBuffer, size: Int, msbMask: Int): Int =
    if (value & MoreBytesMask) != 0 then
      buffer.put((value & RelevantDataBits | msbMask).toByte)
      runEncodingLE(value >>> BitsPerByte, buffer, size + 8, MostSignificantBit)
    else
      buffer.put((value | msbMask).toByte)
      size

  private val BEDecoder = (buffer: BitVector) =>
    runDecodingBE(buffer, MostSignificantBit.toByte, 0L, 0)

  private val LEDecoder = (buffer: BitVector) =>
    runDecodingLE(buffer, MostSignificantBit.toByte, 0L)

  @tailrec
  private def runDecodingBE(
      buffer: BitVector,
      byte: Byte,
      value: Long,
      shift: Int
  ): Attempt[DecodeResult[Long]] =
    if (byte & MostSignificantBit) != 0 then
      if buffer.sizeLessThan(8L) then Attempt.failure(Err.InsufficientBits(8L, buffer.size, Nil))
      else
        val nextByte = buffer.take(8L).toByte(false)
        val nextValue = value | (nextByte & RelevantDataBits) << shift
        runDecodingBE(buffer.drop(8L), nextByte, nextValue, shift + BitsPerByte)
    else Attempt.successful(DecodeResult(value, buffer))

  @tailrec
  private def runDecodingLE(
      buffer: BitVector,
      byte: Byte,
      value: Long
  ): Attempt[DecodeResult[Long]] =
    if (byte & MostSignificantBit) != 0 then
      if buffer.sizeLessThan(8L) then Attempt.failure(Err.InsufficientBits(8L, buffer.size, Nil))
      else
        val nextByte = buffer.take(8L).toByte(false)
        val nextValue = (value << BitsPerByte) | (nextByte & RelevantDataBits)
        runDecodingLE(buffer.drop(8L), nextByte, nextValue)
    else Attempt.successful(DecodeResult(value, buffer))

private object VarLongCodec:
  private val RelevantDataBits = 0x7fL
  private val MoreBytesMask = ~RelevantDataBits.toInt
  private val MostSignificantBit = 0x80
  private val BitsPerByte = 7
