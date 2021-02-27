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

import org.scalacheck.Prop.forAll
import java.security.KeyPairGenerator

import scodec.codecs.*

class SignatureCodecTest extends CodecSuite:

  private val keyPair =
    val keyGen = KeyPairGenerator.getInstance("RSA").nn
    keyGen.initialize(1024)
    keyGen.generateKeyPair.nn

  property("fixedSizeSignature - roundtrip using SHA256withRSA") {
    testFixedSizeSignature(1024 / 8)(SignatureFactory("SHA256withRSA", keyPair))
  }

  property("fixedSizeSignature - roundtrip using MD5") {
    testFixedSizeSignature(16)(ChecksumFactory.digest("MD5"))
  }

  property("fixedSizeSignature - roundtrip using Fletcher") {
    testFixedSizeSignature(2)(ChecksumFactory.fletcher16)
  }

  property("fixedSizeSignature - roundtrip using crc32") {
    testFixedSizeSignature(4)(ChecksumFactory.crc32)
  }

  property("fixedSizeSignature - roundtrip using adler32") {
    testFixedSizeSignature(4)(ChecksumFactory.adler32)
  }

  property("variableSizeSignature - roundtrip using SHA256withRSA") {
    testVariableSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
  }

  property("variableSizeSignature - roundtrip using MD5") {
    testVariableSizeSignature(ChecksumFactory.digest("MD5"))
  }

  protected def testFixedSizeSignature(size: Int)(implicit sf: SignerFactory) =
    val codec = fixedSizeSignature(size) { int32 :: variableSizeBytes(int32, utf8) }
    forAll((n: Int, s: String, x: Int) => roundtrip(codec, (n, s)))

  protected def testVariableSizeSignature(implicit sf: SignerFactory) =
    val codec = variableSizeSignature(uint16) { int32 :: variableSizeBytes(int32, utf8) } :: int32
    forAll((n: Int, s: String, x: Int) => roundtrip(codec, ((n, s), x)))
