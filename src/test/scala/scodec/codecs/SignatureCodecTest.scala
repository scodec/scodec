package scodec
package codecs

import java.security.KeyPairGenerator

class SignatureCodecTest extends CodecSuite {

  private val keyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(1024)
    keyGen.generateKeyPair
  }

  "fixedSizeSignature codec" should {
    "roundtrip using SHA256withRSA" in {
      testFixedSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
    }
  }

  "variableSizeSignature codec" should {
    "roundtrip using SHA256withRSA" in {
      testVariableSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
    }
  }

  protected def testFixedSizeSignature(implicit sf: SignatureFactory) {
    val codec = fixedSizeSignature(1024/8) { int32 ~ variableSizeBytes(int32, utf8) }
    forAll { (n: Int, s: String, x: Int) => roundtrip(codec, n ~ s) }
  }

  protected def testVariableSizeSignature(implicit sf: SignatureFactory) {
    val codec = variableSizeSignature(uint16) { int32 ~ variableSizeBytes(int32, utf8) } ~ int32
    forAll { (n: Int, s: String, x: Int) => roundtrip(codec, n ~ s ~ x) }
  }
}
