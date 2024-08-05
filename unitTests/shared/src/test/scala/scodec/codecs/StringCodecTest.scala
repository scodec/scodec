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

import scodec.bits.{BitVector, ByteVector}

class StringCodecTest extends CodecSuite:

  test("ascii - roundtrip") {
    roundtripAll(ascii, Seq("test", "", "with\ttabs"))
  }

  test("ascii32 - roundtrip") {
    roundtripAll(ascii32, Seq("test", "", "with\ttabs"))
  }

  test("ascii32L - roundtrip") {
    roundtripAll(ascii32L, Seq("test", "", "with\ttabs"))
  }

  test("cstring - roundtrip") {
    roundtripAll(cstring, Seq("test", ""))
  }

  test("cstring - decode up to the first null-character") {
    assertEquals(
      cstring.decode(ascii.encode("hello\u0000").require ++ BitVector.bit(true)),
      Attempt.successful(DecodeResult("hello", BitVector.bit(true)))
    )
  }

  test("cstring - fail decoding with an error when buffer contains bytes unsupported by charset") {
    assertEquals(
      cstring.decode(ascii.encode("0123456789ABCDEF").require),
      Attempt.failure(
        Err.InsufficientBits(136, 128, List("Does not contain a 'NUL' termination byte."))
      )
    )
  }

  test("nulTerminatedString - roundtrip") {
    roundtripAll(nulTerminatedString(utf8), Seq("test", "", "withλ"))
  }

  test("utf8 - roundtrip") {
    roundtripAll(utf8, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("utf8_32 - roundtrip") {
    roundtripAll(utf8_32, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("utf8_32L - roundtrip") {
    roundtripAll(utf8_32L, Seq("test", "", "with\ttabs", "withλ"))
  }

  test("fail encoding with an error when string to encode contains chars unsupported by charset") {
    assertEquals(ascii.encode("λ"), Attempt.failure(Err("US-ASCII cannot encode character 'λ'")))
    assertEquals(
      ascii.encode("Includes a λ"),
      Attempt.failure(
        Err("US-ASCII cannot encode character 'λ'")
      )
    )
  }

  test("fail decoding with an error when buffer contains bytes unsupported by charset") {
    assertEquals(
      utf8.decode(BitVector(ByteVector.fromValidHex("0xf4ffffff"))),
      Attempt.failure(
        Err("UTF-8 cannot decode string from '0xf4ffffff'")
      )
    )
  }
