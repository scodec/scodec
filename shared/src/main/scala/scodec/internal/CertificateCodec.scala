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

import java.io.ByteArrayInputStream
import java.security.cert.{Certificate, CertificateException, CertificateFactory}

import scodec.bits.BitVector

/**
  * Codec that supports encoding and decoding of [[java.security.cert.Certificate]]s using their default encoding.
  */
private[scodec] final class CertificateCodec(certType: String) extends Codec[Certificate]:

  def sizeBound = SizeBound.unknown

  def encode(cert: Certificate) =
    Attempt.successful(BitVector(cert.getEncoded.nn))

  def decode(buffer: BitVector) =
    try
      val factory = CertificateFactory.getInstance(certType).nn
      val cert = factory.generateCertificate(new ByteArrayInputStream(buffer.toByteArray)).nn
      Attempt.successful(DecodeResult(cert, BitVector.empty))
    catch
      case e: CertificateException =>
        Attempt.failure(Err("Failed to decode certificate: " + e.getMessage))

  override def toString = s"certificate($certType)"
