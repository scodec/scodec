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
import scala.annotation.tailrec

private[scodec] final class PaddedFixedSizeCodec[A](
    size: Long,
    codec: Codec[A],
    padCodec: Long => Codec[Unit]
) extends Codec[A]:
  override def sizeBound = SizeBound.exact(size)

  override def encode(a: A) =
    for
      encoded <- codec.encode(a)
      result <-
        if encoded.size > size then
          Attempt.failure(
            Err(s"[$a] requires ${encoded.size} bits but field is fixed size of $size bits")
          )
        else if encoded.size == size then
          Attempt.successful(encoded)
        else
          val pc = padCodec(size - encoded.size)
          pc.encode(()) match
            case Attempt.Successful(padding) =>
              @tailrec def pad(bits: BitVector): Attempt[BitVector] =
                if bits.size == size then Attempt.successful(bits)
                else if bits.size < size then pad(bits ++ padding)
                else
                  Attempt.failure(
                    Err(s"padding overflows fixed size field of $size bits").pushContext("padding")
                  )
              pad(encoded)

            case Attempt.Failure(err) =>
              Attempt.failure(err.pushContext("padding"))
    yield result

  override def decode(buffer: BitVector) =
    buffer.acquire(size) match
      case Left(_) => Attempt.failure(Err.insufficientBits(size, buffer.size))
      case Right(b) =>
        codec.decode(b) match
          case e @ Attempt.Failure(_) => e
          case Attempt.Successful(DecodeResult(res, rest)) =>
            val pc = padCodec(rest.size)
            @tailrec def validate(bits: BitVector): Attempt[DecodeResult[A]] =
              if bits.isEmpty then
                Attempt.successful(DecodeResult(res, buffer.drop(size)))
              else
                pc.decode(bits) match
                  case Attempt.Failure(err)                        => Attempt.failure(err.pushContext("padding"))
                  case Attempt.Successful(DecodeResult(_, remain)) => validate(remain)
            validate(rest)

  override def toString = s"paddedFixedSizeBits($size, $codec)"
