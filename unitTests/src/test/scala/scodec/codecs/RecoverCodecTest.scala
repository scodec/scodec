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

import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class RecoverCodecTest extends CodecSuite:

  property("recover - always code a value for true value") {
    forAll { (i: Int) =>
      val codec = recover(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b2, rest)) =
        (for
          bits <- codec.encode(true)
          result <- codec.decode(bits)
        yield result): @unchecked
      assert(rest.isEmpty)
      assert(b2)
    }
  }

  property("recover - always code an empty vector for false value") {
    forAll { (i: Int) =>
      val codec = recover(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b2, rest)) =
        (for
          bits <- codec.encode(false)
          result <- codec.decode(bits)
        yield result): @unchecked
      assert(rest.isEmpty)
      assert(!b2)
    }
  }

  property("recover - decode a false when target codec fails to decode") {
    forAll { (i: Int) =>
      val codec = recover(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.empty): @unchecked
      assert(rest.isEmpty)
      assert(!b)
    }
  }

  property("recover - decode the wrong value as false and backtrack") {
    forAll { (i1: Int, i2: Int) =>
      if (i1 != i2 && i1 >= 0 && i2 >= 0)
        val codec = recover(constant(BitVector.fromInt(i1)))
        val Attempt.Successful(DecodeResult(b, rest)) =
          codec.decode(BitVector.fromInt(i2)): @unchecked
        assertEquals(rest, BitVector.fromInt(i2))
        assert(!b)
    }
  }

  property("lookahead - always code a value for true value") {
    forAll { (i: Int) =>
      val codec = lookahead(constant(BitVector.fromInt(i)))
      val Attempt.Successful(encoded, DecodeResult(b2, rest)) =
        (for
          bits <- codec.encode(true)
          result <- codec.decode(bits)
        yield (bits, result)): @unchecked
      assertEquals(rest, encoded)
      assert(b2)
    }
  }

  property("lookahead - always code an empty vector for false value") {
    forAll { (i: Int) =>
      val codec = lookahead(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b2, rest)) =
        (for
          bits <- codec.encode(false)
          result <- codec.decode(bits)
        yield result): @unchecked
      assert(rest.isEmpty)
      assert(!b2)
    }
  }

  property("lookahead - decode a false when target codec fails to decode") {
    forAll { (i: Int) =>
      val codec = lookahead(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.empty): @unchecked
      assert(rest.isEmpty)
      assert(!b)
    }
  }

  property("lookahead - decode the wrong value as false and backtrack") {
    forAll { (i1: Int, i2: Int) =>
      if (i1 != i2 && i1 >= 0 && i2 >= 0)
        val codec = lookahead(constant(BitVector.fromInt(i1)))
        val Attempt.Successful(DecodeResult(b, rest)) =
          codec.decode(BitVector.fromInt(i2)): @unchecked
        assertEquals(rest, BitVector.fromInt(i2))
        assert(!b)
    }
  }
