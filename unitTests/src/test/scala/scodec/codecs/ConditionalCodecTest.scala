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

import scodec._
import scodec.bits._

class ConditionalCodecTest extends CodecSuite {

  test("not evaluate if condition is false when encoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(false, retCodec).encode(Some("00"))

    assert(!called)
    assertEquals(result, Attempt.successful(BitVector.empty))
  }

  test("evaluate if condition is true when encoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(true, retCodec).encode(Some("00"))

    assert(called)
    assertEquals(result, Attempt.successful(hex"3030".bits))
  }

  test("not evaluate if condition is false when decoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(false, retCodec).decode(hex"3030".bits)

    assert(!called)
    assertEquals(result, Attempt.successful(DecodeResult(None, hex"3030".bits)))
  }

  test("evaluate if condition is true when decoding") {
    var called = false

    def retCodec: Codec[String] = {
      called = true
      ascii
    }

    val result = conditional(true, retCodec).decode(hex"3030".bits)

    assert(called)
    assertEquals(result, Attempt.successful(DecodeResult(Some("00"), BitVector.empty)))
  }

}
