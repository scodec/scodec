package scodec
package codecs

import javax.crypto.KeyGenerator
import javax.crypto.spec.IvParameterSpec

import scodec.bits.ByteVector

import org.scalacheck.Prop.forAll

class CipherCodecTest extends CodecSuite {

  private val secretKey = {
    val keyGen = KeyGenerator.getInstance("AES").nn
    keyGen.init(128)
    keyGen.generateKey.nn
  }

  private val iv = new IvParameterSpec(ByteVector.low(16).toArray)

  property("roundtrip with AES/ECB/PKCS5Padding") {
    testWithCipherFactory(using CipherFactory("AES/ECB/PKCS5Padding", secretKey))
  }
  property("roundtrip with AES/CBC/PKCS5Padding") {
    testWithCipherFactory(using CipherFactory("AES/CBC/PKCS5Padding", secretKey, iv))
  }

  protected def testWithCipherFactory(using CipherFactory) = {
    val codec = encrypted { int32 :: utf8 }
    forAll((n: Int, s: String) => roundtrip(codec, (n, s)))
  }
}
