package scodec
package codecs

import java.io.ByteArrayInputStream
import java.security.cert.{Certificate, CertificateException, CertificateFactory}

import scodec.bits.BitVector

/**
 * Codec that supports encoding and decoding of [[java.security.cert.Certificate]]s using their default encoding.
 */
private[codecs] final class CertificateCodec(certType: String) extends Codec[Certificate] {

  def encode(cert: Certificate) =
    EncodeResult.successful(BitVector(cert.getEncoded))

  def decode(buffer: BitVector) = {
    try {
      val factory = CertificateFactory.getInstance(certType)
      val cert = factory.generateCertificate(new ByteArrayInputStream(buffer.toByteArray))
      DecodeResult.successful(cert, BitVector.empty)
    } catch {
      case e: CertificateException =>
        DecodeResult.failure(Err("Failed to decode certificate: " + e.getMessage))
    }
  }

  override def toString = s"certificate($certType)"
}
