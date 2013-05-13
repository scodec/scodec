package scodec

import javax.crypto.{Cipher, KeyGenerator}
import javax.crypto.spec.IvParameterSpec

import Codecs._


class CipherCodecTest extends CodecSuite {

  private val secretKey = {
    val keyGen = KeyGenerator.getInstance("AES")
    keyGen.init(128)
    keyGen.generateKey
  }

  private val iv = new IvParameterSpec(ByteVector.low(16).toArray)

  test("AES/ECB/PKCS5Padding") {
    testWithCipherFactory(CipherFactory("AES/ECB/PKCS5Padding", secretKey))
  }

  test("AES/CBC/PKCS5Padding") {
    testWithCipherFactory(CipherFactory("AES/CBC/PKCS5Padding", secretKey, iv))
  }

  protected def testWithCipherFactory(implicit cf: CipherFactory) {
    val codec = encrypted { int32 ~ utf8 }
    forAll { (n: Int, s: String) => roundtrip(codec, (n, s)) }
  }
}
