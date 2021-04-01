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

import java.security.MessageDigest
import java.util.Arrays
import java.util.zip.{Adler32, CRC32, Checksum}
import scodec.bits.ByteVector

/**
  * Creates checksum implementations of [[SignerFactory]].
  */
object ChecksumFactory:

  /** Creates a `java.security.Digest` factory for the specified algorithm. */
  def digest(algorithm: String): SignerFactory = new ChecksumFactory {
    def newSigner: Signer = new DigestSigner(MessageDigest.getInstance(algorithm).nn)
  }

  /** Signer factory that does not have a distinct verifier. */
  private trait ChecksumFactory extends SignerFactory:
    def newVerifier: Signer = newSigner

  /** Fletcher-16 checksum. */
  val fletcher16: SignerFactory = new ChecksumFactory {
    def newSigner = new Fletcher16Checksum
  }

  /** CRC-32 checksum. */
  val crc32: SignerFactory = new ChecksumFactory {
    def newSigner = new ZipChecksumSigner(new CRC32())
  }

  /** Adler-32 checksum. */
  val adler32: SignerFactory = new ChecksumFactory {
    def newSigner = new ZipChecksumSigner(new Adler32())
  }

  /** xor checksum. */
  val xor: SignerFactory = new ChecksumFactory {
    def newSigner = new XorSigner
  }

  /** `java.security.Digest` implementation of Signer. */
  private class DigestSigner(impl: MessageDigest) extends Signer:
    def update(data: Array[Byte]): Unit = impl.update(data)
    def sign: Array[Byte] = impl.digest.nn
    def verify(signature: Array[Byte]): Boolean = MessageDigest.isEqual(impl.digest(), signature)

  /** http://en.wikipedia.org/wiki/Fletcher's_checksum */
  private class Fletcher16Checksum extends Signer:
    var checksum = (0, 0)
    def update(data: Array[Byte]): Unit =
      checksum = data.foldLeft(checksum) { (p, b) =>
        val lsb = (p._2 + (0xff & b)) % 255
        ((p._1 + lsb) % 255, lsb)
      }
    def sign: Array[Byte] = Array(checksum._1.asInstanceOf[Byte], checksum._2.asInstanceOf[Byte])
    def verify(signature: Array[Byte]): Boolean = Arrays.equals(sign, signature)

  /** `java.util.zip.Checksum` implementation of Signer. */
  private class ZipChecksumSigner(impl: Checksum) extends Signer:
    def update(data: Array[Byte]): Unit = impl.update(data, 0, data.length)
    def sign: Array[Byte] = ByteVector.fromLong(impl.getValue()).drop(4).toArray
    def verify(signature: Array[Byte]): Boolean = MessageDigest.isEqual(sign, signature)

  private class XorSigner extends Signer:
    var data: Array[Byte] = null

    def update(data: Array[Byte]): Unit = this.data = data
    def sign: Array[Byte] = Array(data.reduce((b1, b2) => (b1 ^ b2).toByte))
    def verify(signature: Array[Byte]): Boolean = sign.sameElements(signature)
