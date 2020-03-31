package scodec
package codecs

import org.scalacheck.Prop.forAll
import java.security.KeyPairGenerator

class SignatureCodecTest extends CodecSuite {

  private val keyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA").nn
    keyGen.initialize(1024)
    keyGen.generateKeyPair.nn
  }

  property("fixedSizeSignature - roundtrip using SHA256withRSA") {
    testFixedSizeSignature(1024 / 8)(SignatureFactory("SHA256withRSA", keyPair))
  }

  property("fixedSizeSignature - roundtrip using MD5") {
    testFixedSizeSignature(16)(ChecksumFactory.digest("MD5"))
  }

  property("fixedSizeSignature - roundtrip using Fletcher") {
    testFixedSizeSignature(2)(ChecksumFactory.fletcher16)
  }

  property("fixedSizeSignature - roundtrip using crc32") {
    testFixedSizeSignature(4)(ChecksumFactory.crc32)
  }

  property("fixedSizeSignature - roundtrip using adler32") {
    testFixedSizeSignature(4)(ChecksumFactory.adler32)
  }

  property("variableSizeSignature - roundtrip using SHA256withRSA") {
    testVariableSizeSignature(SignatureFactory("SHA256withRSA", keyPair))
  }

  property("variableSizeSignature - roundtrip using MD5") {
    testVariableSizeSignature(ChecksumFactory.digest("MD5"))
  }

  protected def testFixedSizeSignature(size: Int)(implicit sf: SignerFactory) = {
    val codec = fixedSizeSignature(size) { int32 :: variableSizeBytes(int32, utf8) }
    forAll((n: Int, s: String, x: Int) => roundtrip(codec, (n, s)))
  }

  protected def testVariableSizeSignature(implicit sf: SignerFactory) = {
    val codec = variableSizeSignature(uint16) { int32 :: variableSizeBytes(int32, utf8) } :: int32
    forAll((n: Int, s: String, x: Int) => roundtrip(codec, ((n, s), x)))
  }
}
