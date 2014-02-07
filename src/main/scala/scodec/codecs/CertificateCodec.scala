package scodec
package codecs

import scalaz.{\/-, -\/}
import java.io.ByteArrayInputStream
import java.security.cert.{Certificate, CertificateException, CertificateFactory}


/**
 * Codec that supports encoding and decoding of [[java.security.cert.Certificate]]s using their default encoding.
 */
private[codecs] final class CertificateCodec(certType: String) extends Codec[Certificate] {

  def encode(cert: Certificate) =
    \/-(BitVector(cert.getEncoded))

  def decode(buffer: BitVector) = {
    try {
      val factory = CertificateFactory.getInstance(certType)
      val cert = factory.generateCertificate(new ByteArrayInputStream(buffer.toByteArray))
      \/-((BitVector.empty, cert))
    } catch {
      case e: CertificateException =>
        -\/("Failed to decode certificate: " + e.getMessage)
    }
  }

  override def toString = s"certificate($certType)"
}
