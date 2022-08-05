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

import org.scalacheck.*
import Prop.forAll
import scodec.bits.ByteVector

/** Test the Fletcher checksum functionality. */
class FletcherChecksumTest extends CodecSuite:

  /**  http://en.wikipedia.org/wiki/Fletcher's_checksum
    *
    *  "Example calculation of the Fletcher-16 checksum"
    */
  test("wikipedia") {
    val signer = ChecksumFactory.fletcher16.newSigner
    signer.update(Array(0x01, 0x02))
    assert(signer.verify(Array(0x04, 0x03)))
  }

  property("0xAA * (3N+2) => Array(0x0,0x55)") {
    forAll(Gen.posNum[Int])((n: Int) => pattern(n, 2, Array(0x0, 0x55)))
  }

  property("0xAA * (3N + 1) => Array(0xAA, 0xAA)") {
    forAll(Gen.posNum[Int]) { (n: Int) =>
      pattern(n, 1, Array(0xaa.asInstanceOf[Byte], 0xaa.asInstanceOf[Byte]))
    }
  }

  property("0xAA * (3N) => Array(0x0, 0x0)") {
    forAll(Gen.posNum[Int])((n: Int) => pattern(n, 0, Array(0x0, 0x0)))
  }

  private def pattern(n: Int, delta: Int, expected: Array[Byte]): Unit =
    val signer = ChecksumFactory.fletcher16.newSigner
    signer.update(ByteVector.fill(3L * n + delta)(0xaa).toArray)
    assert(signer.verify(expected))
    ()
