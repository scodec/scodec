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

package scodec.codecs

import scodec.*
import scodec.bits.{BitVector, ByteVector}

/**
  * Provides methods to create a "checksum codec" (encodes a bit-range to a bit-checksum and decodes bits to a bit-range).
  */
object ChecksumCodec {

  /**
    * Returns a codec that encodes a bit-range to a bit-checksum and decodes bits to a bit-range.
    *
    * @param encoder encodes a bit-range to a bit-checksum
    * @param range decodes the size of a bit-range
    * @return
    */
  def apply(encoder: Encoder[BitVector], range: Decoder[Long]): Codec[BitVector] =
    Codec(
      encoder,
      Decoder[BitVector](bits =>
        range
          .decode(bits)
          .flatMap(size =>
            bits.consumeThen(size.value)(
              e => Attempt.failure(Err.InsufficientBits(size.value, bits.size, List(e))),
              (range, remainder) => Attempt.successful(DecodeResult(range, remainder))
            )
          )
      )
    )

  /**
    * Returns a codec that encodes a bit-range to a bit-checksum and decodes bits to a bit-range.
    *
    * @param encoder encodes a bit-range to a bit-checksum
    * @param range decodes the (un-padded) size of a bit-range
    * @param padding size padding for the bit-range
    * @return
    */
  def apply(encoder: Encoder[BitVector], range: Decoder[Long], padding: Long): Codec[BitVector] =
    apply(encoder, range.map(_ + padding))

  /**
    * Returns a codec that encodes a bit-range to a bit-checksum and decodes bits to a bit-range.
    *
    * @param encoder encodes a byte-range to a byte-checksum
    * @param range decodes the (un-padded) size of a byte-range
    * @param padding size padding for the byte-range
    * @return
    */
  def apply(encoder: Encoder[ByteVector], range: Decoder[Int], padding: Int): Codec[BitVector] =
    apply(encoder.contramap[BitVector](_.bytes), range.map(8L * _), 8L * padding)

  /**
    * Returns a codec that encodes a bit-range to a bit-checksum and decodes bits to a bit-range.
    *
    * @param length the bit-length of the checksum
    * @param f computes bit-checksum
    * @param range decodes the (un-padded) size of a bit-range
    * @param padding size padding for the bit-range
    * @return
    */
  def apply(
      length: Long,
      f: BitVector => BitVector,
      range: Decoder[Long],
      padding: Long
  ): Codec[BitVector] =
    apply(
      new Encoder[BitVector] {
        def encode(value: BitVector): Attempt[BitVector] = Attempt.successful(f(value))
        def sizeBound: SizeBound = SizeBound.exact(length)
      },
      range,
      padding
    )

  /**
    * Returns a codec that encodes a bit-range to a bit-checksum and decodes bits to a bit-range.
    *
    * @param length the byte-length of the checksum
    * @param f computes byte-checksum
    * @param range decodes the (un-padded) size of a byte-range
    * @param padding size padding for the byte-range
    * @return
    */
  def apply(
      length: Int,
      f: ByteVector => ByteVector,
      range: Decoder[Int],
      padding: Int
  ): Codec[BitVector] =
    apply(8L * length, (bits: BitVector) => f(bits.bytes).bits, range.map(8L + _), 8L * padding)

  /**
    * Returns a codec that encodes a bit-range to an XORed bit-checksum and decodes bits to a bit-range.
    *
    * @param length the bit-length of the checksum
    * @param range decodes the (un-padded) size of a bit-range
    * @param padding size padding for the bit-range
    * @return
    */
  def xor(length: Long, range: Decoder[Long], padding: Long): Codec[BitVector] =
    apply(
      length,
      (bits: BitVector) => BitVector.GroupedOp(bits).grouped(length).foldLeft(BitVector.low(length))(_.xor(_)),
      range,
      padding
    )

  /**
    * Returns a codec that encodes a bit-range to an XORed bit-checksum and decodes bits to a bit-range.
    *
    * @param length the byte-length of the checksum
    * @param range decodes the (un-padded) size of a byte-range
    * @param padding size padding for the byte-range
    * @return
    */
  def xor(length: Int, range: Decoder[Int], padding: Int): Codec[BitVector] =
    xor(8L * length, range.map(8L * _), 8L * padding)

  case class Mismatch(
      bits: BitVector,
      expected: BitVector,
      actual: BitVector,
      context: List[String] = Nil
  ) extends Err {

    def message: String = s"checksum mismatch for bits: $bits, expected: $expected, actual: $actual"

    def pushContext(ctx: String): Err = copy(context = ctx :: context)
  }

}
