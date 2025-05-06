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

class FatCodecTest extends CodecSuite:
  case class Fat(
      v1: Int,
      v2: Int,
      v3: Int,
      v4: Int,
      v5: Int,
      v6: Int,
      v7: Int,
      v8: Int,
      v9: Int,
      v10: Int,
      v11: Int,
      v12: Int,
      v13: Int,
      v14: Int,
      v15: Int,
      v16: Int,
      v17: Int,
      v18: Int,
      v19: Int,
      v20: Int,
      v21: Int,
      v22: Int,
      v23: Int,
      v24: Int,
      v25: Int,
      v26: Int,
      v27: Int,
      v28: Int,
      v29: Int,
      v30: Int
  ) derives Codec

  test("derivation of fat case class") {
    roundtrip(
      Codec[Fat],
      Fat(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30
      )
    )
  }

  test("support dropping all Unit values out of a tuple codec") {
    def ign(size: Int) = scodec.codecs.ignore(size.toLong)

    val codec =
      (uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 ::
        ign(8) :: uint8 :: ign(8)).dropUnits.as[Fat]

    roundtrip(
      codec,
      Fat(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30
      )
    )
  }
