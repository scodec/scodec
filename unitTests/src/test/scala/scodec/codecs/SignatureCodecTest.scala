package scodec
package codecs

import java.security.KeyPairGenerator

class SignatureCodecTest extends CodecSuite {

  private val keyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA").nn
    keyGen.initialize(1024)
    keyGen.generateKeyPair.nn
  }

  "fixedSizeSignature codec" should {
    "roundtrip using SHA256withRSA" in {
      testFixedSizeSignature(1024 / 8)(SignatureFactory("SHA256withRSA", keyPair))
    }

    "roundtrip using MD5" in {
      testFixedSizeSignature(16)(ChecksumFactory.digest("MD5"))
    }

    "roundtrip using Fletcher" in {
      testFixedSizeSignature(2)(ChecksumFactory.fletcher16)
    }

    "roundtrip using crc32" in {
      testFixedSizeSignature(4)(ChecksumFactory.crc32)
    }

    "roundtrip using adler32" in {
      testFixedSizeSignature(4)(ChecksumFactory.adler32)
    }
  }

  "variableSizeSignature codec" should {
    "roundtrip using SHA256withRSA" in {
      testVariableSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
    }
  }

  "variableSizeSignature codec" should {
    "roundtrip using MD5" in {
      testVariableSizeSignature(ChecksumFactory.digest("MD5"))
    }
  }

  protected def testFixedSizeSignature(size: Int)(implicit sf: SignerFactory): Unit = {
    val codec = fixedSizeSignature(size) { int32 :: variableSizeBytes(int32, utf8) }
    forAll((n: Int, s: String, x: Int) => roundtrip(codec, (n, s)))
  }

  protected def testVariableSizeSignature(implicit sf: SignerFactory): Unit = {
    val codec = variableSizeSignature(uint16) { int32 :: variableSizeBytes(int32, utf8) } :: int32
    forAll((n: Int, s: String, x: Int) => roundtrip(codec, ((n, s), x)))
  }
}
