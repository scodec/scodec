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

class SizeBoundTest extends CodecSuite {

  test("support addition of bounds") {
    assertEquals(SizeBound.exact(1) + SizeBound.exact(2), SizeBound.exact(3))
    assertEquals(SizeBound.exact(1) + SizeBound.unknown, SizeBound.atLeast(1))
    assertEquals(SizeBound.exact(1) + SizeBound.atLeast(2), SizeBound.atLeast(3))
    assertEquals(SizeBound.exact(1) + SizeBound.atMost(2), SizeBound.bounded(1, 3))
  }

  test("support multiplying a bound by a scalar") {
    assertEquals(SizeBound.exact(1) * 3, SizeBound.exact(3))
    assertEquals(SizeBound.unknown * 3, SizeBound.unknown)
    assertEquals(SizeBound.atLeast(2) * 6, SizeBound.atLeast(12))
    assertEquals(SizeBound.bounded(1, 5) * 10, SizeBound.bounded(10, 50))
  }

  test("support ORing of bounds") {
    assertEquals(SizeBound.exact(1) | SizeBound.exact(2), SizeBound.bounded(1, 2))
    assertEquals(SizeBound.exact(1) | SizeBound.unknown, SizeBound.unknown)
    assertEquals(SizeBound.unknown | SizeBound.exact(1), SizeBound.unknown)
    assertEquals(SizeBound.exact(1) | SizeBound.atLeast(2), SizeBound.atLeast(1))
    assertEquals(SizeBound.exact(1) | SizeBound.atMost(2), SizeBound.bounded(0, 2))
  }

  test("support choice (i.e., ORing a collection of bounds together)") {
    assertEquals(SizeBound.choice(List(SizeBound.exact(1), SizeBound.exact(2), SizeBound.exact(3))), SizeBound.bounded(1, 3))
    assertEquals(SizeBound.choice(Nil), SizeBound.exact(0))
  }

  test("prevent creation of non-sensical bounds") {
    intercept[IllegalArgumentException] { SizeBound(1, Some(0)) }
  }
}
