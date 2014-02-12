package scodec
package codecs

import scalaz.{\/, \/-, -\/}
import scalaz.syntax.monad._

import java.security.Key
import java.security.spec.AlgorithmParameterSpec
import javax.crypto.{Cipher, IllegalBlockSizeException, BadPaddingException}

import scodec.bits.BitVector

/**
 * Represents the ability to create a [[Cipher]] for encryption or decryption.
 *
 * Used in conjunction with [[CipherCodec]]. Typically provided implicitly to all cipher codecs in a larger codec.
 */
trait CipherFactory {

  /** Creates a cipher initialized for encryption. */
  def newEncryptCipher: Cipher

  /** Creates a cipher initialized for decryption. */
  def newDecryptCipher: Cipher
}

object CipherFactory {

  /**
   * Creates a cipher factory for the specified transformation (via `Cipher.getInstance(transformation)`)
   * and using the specified functions for initializing for encryption and decryption respectively.
   */
  def apply(transformation: String, initForEncryption: Cipher => Unit, initForDecryption: Cipher => Unit): CipherFactory =
    new SimpleCipherFactory(transformation, initForEncryption, initForDecryption)

  /**
   * Creates a cipher factory for the specified transformation (via `Cipher.getInstance(transformation)`).
   *
   * Ciphers are initialized with the specified key only.
   */
  def apply(transformation: String, key: Key): CipherFactory =
    new SimpleCipherFactory(transformation, _.init(Cipher.ENCRYPT_MODE, key), _.init(Cipher.DECRYPT_MODE, key))

  /**
   * Creates a cipher factory for the specified transformation (via `Cipher.getInstance(transformation)`).
   *
   * Ciphers are initialized with the specified key and algorithm parameter specification.
   */
  def apply(transformation: String, key: Key, spec: AlgorithmParameterSpec): CipherFactory =
    new SimpleCipherFactory(transformation, _.init(Cipher.ENCRYPT_MODE, key, spec), _.init(Cipher.DECRYPT_MODE, key, spec))

  private class SimpleCipherFactory(transformation: String, initForEncryption: Cipher => Unit, initForDecryption: Cipher => Unit) extends CipherFactory {

    private def newCipher: Cipher = Cipher.getInstance(transformation)

    def newEncryptCipher: Cipher = {
      val cipher = newCipher
      initForEncryption(cipher)
      cipher
    }

    def newDecryptCipher: Cipher = {
      val cipher = newCipher
      initForDecryption(cipher)
      cipher
    }
  }

}

/**
 * Codec that encrypts and decrypts using a `javax.crypto.Cipher`.
 *
 * Encoding a value of type A is delegated to the specified codec and the resulting bit vector is encrypted
 * with a cipher provided by the implicit cipher factory.
 *
 * Decoding first decrypts all of the remaining bits and then decodes the decrypted bits with the
 * specified codec. Successful decoding always returns no remaining bits, even if the specified
 * codec does not consume all decrypted bits.
 */
private[codecs] final class CipherCodec[A](codec: Codec[A])(implicit cipherFactory: CipherFactory) extends Codec[A] {

  override def encode(a: A) =
    codec.encode(a) >>= encrypt

  private def encrypt(bits: BitVector): String \/ BitVector = {
    val blocks = bits.toByteArray
    try {
      val encrypted = cipherFactory.newEncryptCipher.doFinal(blocks)
      \/-(BitVector(encrypted))
    } catch {
      case e: IllegalBlockSizeException => -\/(s"Failed to encrypt: invalid block size ${blocks.size}")
    }
  }

  override def decode(buffer: BitVector) =
    (decrypt(buffer) >>= codec.decode) map { case (remaining, a) => (BitVector.empty, a) }

  private def decrypt(buffer: BitVector): String \/ BitVector = {
    val blocks = buffer.toByteArray
    try {
      val decrypted = cipherFactory.newDecryptCipher.doFinal(blocks)
      \/-(BitVector(decrypted))
    } catch {
      case e @ (_: IllegalBlockSizeException | _: BadPaddingException) =>
        -\/("Failed to decrypt: " + e.getMessage)
    }
  }

  override def toString = s"cipher($codec)"
}
