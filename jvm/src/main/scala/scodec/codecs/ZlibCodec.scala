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

import java.util.zip.{DataFormatException, Inflater}

import scodec.bits.{BitVector, ByteVector}

private[codecs] class ZlibCodec[A](
    codec: Codec[A],
    level: Int,
    strategy: Int,
    nowrap: Boolean,
    chunkSize: Int
) extends Codec[A]:

  def sizeBound = SizeBound.unknown

  def encode(a: A) =
    codec.encode(a).map(b => b.deflate(level, strategy, nowrap, chunkSize))

  def decode(b: BitVector) =
    inflate(b, chunkSize).fold(
      e => Attempt.failure(Err.fromThrowable(e)),
      bb => codec.decode(bb.value.bits).map(_.mapRemainder(_ => bb.remainder))
    )

  private def inflate(
      b: BitVector,
      chunkSize: Int
  ): Either[DataFormatException, DecodeResult[ByteVector]] =
    if b.isEmpty then Right(DecodeResult(b.bytes, b))
    else
      val arr = b.bytes.toArray

      val inflater = new Inflater(false)
      try
        inflater.setInput(arr)
        try
          val buffer = new Array[Byte](chunkSize.min(arr.length))
          def loop(acc: ByteVector): ByteVector =
            if inflater.finished || inflater.needsInput then acc
            else
              val count = inflater.inflate(buffer)
              loop(acc ++ ByteVector(buffer, 0, count))
          val inflated = loop(ByteVector.empty)
          if inflater.finished then Right(DecodeResult(inflated, b.drop(inflater.getBytesRead * 8)))
          else
            Left(
              new DataFormatException(
                "Insufficient data -- inflation reached end of input without completing inflation - " + inflated
              )
            )
        catch case e: DataFormatException => Left(e)
      finally inflater.end()
