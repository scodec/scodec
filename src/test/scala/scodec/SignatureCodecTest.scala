package scodec

import java.security.KeyPairGenerator

import Codecs._


class SignatureCodecTest extends CodecSuite {

  private val keyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(1024)
    keyGen.generateKeyPair
  }

  test("SHA256withRSA") {
    testWithSignatureFactory(SignatureFactory("SHA256withRSA", keyPair))
  }

  protected def testWithSignatureFactory(implicit sf: SignatureFactory) {
    val codec = signed { int32 ~ variableSizeBytes(int32, utf8) }
    forAll { (n: Int, s: String) => roundtrip(codec, (n, s)) }
  }
}
