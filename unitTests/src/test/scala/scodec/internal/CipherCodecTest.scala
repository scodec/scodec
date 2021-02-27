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

import javax.crypto.KeyGenerator
import javax.crypto.spec.IvParameterSpec

import scodec.bits.ByteVector
import scodec.codecs.*

import org.scalacheck.Prop.forAll

class CipherCodecTest extends CodecSuite:

  private val secretKey =
    val keyGen = KeyGenerator.getInstance("AES").nn
    keyGen.init(128)
    keyGen.generateKey.nn

  private val iv = new IvParameterSpec(ByteVector.low(16).toArray)

  property("roundtrip with AES/ECB/PKCS5Padding") {
    testWithCipherFactory(CipherFactory("AES/ECB/PKCS5Padding", secretKey))
  }
  property("roundtrip with AES/CBC/PKCS5Padding") {
    testWithCipherFactory(CipherFactory("AES/CBC/PKCS5Padding", secretKey, iv))
  }

  protected def testWithCipherFactory(cipherFactory: CipherFactory) =
    val codec = encrypted(int32 :: utf8, cipherFactory)
    forAll((n: Int, s: String) => roundtrip(codec, (n, s)))
