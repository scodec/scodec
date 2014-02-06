package scodec
package codecs

import java.security.KeyPairGenerator

class SignatureCodecTest extends CodecSuite {

  private val keyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(1024)
    keyGen.generateKeyPair
  }

  test("SHA256withRSA fixed size") {
    testFixedSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
  }

  test("SHA256withRSA variable size") {
    testVariableSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
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
