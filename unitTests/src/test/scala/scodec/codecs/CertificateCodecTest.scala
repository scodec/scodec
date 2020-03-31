package scodec
package codecs

import java.io.ByteArrayInputStream
import java.security.KeyPairGenerator
import java.security.cert.{CertificateFactory, X509Certificate}
import java.util.Date
import org.bouncycastle.asn1.x500.X500Name
import org.bouncycastle.cert.jcajce._
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder

class CertificateCodecTest extends CodecSuite {

  private val keyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA").nn
    keyGen.initialize(1024)
    keyGen.generateKeyPair.nn
  }

  val aCert: X509Certificate = {
    val issuer = new X500Name("CN=Test")
    val serialNum = BigInt(1).bigInteger
    val notBefore = new Date(System.currentTimeMillis - 1000000)
    val notAfter = new Date()
    val subject = issuer
    val bldr = new JcaX509v3CertificateBuilder(
      issuer,
      serialNum,
      notBefore,
      notAfter,
      subject,
      keyPair.getPublic
    )
    val signer = new JcaContentSignerBuilder("SHA1withRSA").build(keyPair.getPrivate)
    val holder = bldr.build(signer).nn
    CertificateFactory
      .getInstance("X.509")
      .generateCertificate(new ByteArrayInputStream(holder.getEncoded))
      .asInstanceOf[X509Certificate]
  }

  test("roundtrip") {
    roundtrip(x509Certificate, aCert)
  }
}
