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

/** Zig-zag encoding for negative numbers (defined by Google Protocol Buffers but also used in other protocols like Kafka).
  *
  * @see https://en.wikipedia.org/wiki/Variable-length_quantity#Zigzag_encoding
  * @see https://developers.google.com/protocol-buffers/docs/encoding#signed-ints
  */
private[codecs] final class VarIntZigZagCodec(ordering: ByteOrdering) extends Codec[Int]:
  import VarIntZigZagCodec.*

  private val codec = new VarIntCodec(ordering).xmap(zigZagDecode, zigZagEncode)

  override val sizeBound =
    codec.sizeBound

  override def encode(i: Int) =
    codec.encode(i)

  override def decode(buffer: BitVector) =
    codec.decode(buffer)

  override def toString = "variable-length zig-zag signed integer"

private object VarIntZigZagCodec:
  private val zigZagEncode = (value: Int) => (value << 1) ^ (value >> 31)
  private val zigZagDecode = (value: Int) => (value >>> 1) ^ -(value & 1)
