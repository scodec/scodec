package scodec
package codecs

import java.security.MessageDigest
import java.util.Arrays
import java.util.zip.{ CRC32, Adler32, Checksum }
import scodec.bits.ByteVector

/**
 * Create "checksum" implementations for [[SignerFactory]]
 * @group checksum
 */
object ChecksumFactory {
  /**
   * Creates a `java.security.Digest` factory for the specified algorithm
   * 
   */
  def digest(algorithm: String): SignerFactory =
    new DigestFactory(algorithm)

  /**
   * Fletcher-16 checksum
   */
  val fletcher16: SignerFactory = new ChecksumFactory {
    def newSigner = new Fletcher16Checksum
  }

  /**
   * CRC-32 checksum
   */
  val crc32: SignerFactory = new ChecksumFactory {
    def newSigner = new ZipChecksumSigner(new CRC32())
  }

  /**
   * Adler-32 checksum
   */
  val adler32: SignerFactory = new ChecksumFactory {
    def newSigner = new ZipChecksumSigner(new Adler32())
  }

  /**
   * `java.security.Digest` implementation of Signer
   */
  private class DigestSigner(impl: MessageDigest) extends Signer {
    def update(data: Array[Byte]): Unit = impl.update(data)
    def sign: Array[Byte] = impl.digest
    def verify(signature: Array[Byte]): Boolean = MessageDigest.isEqual(impl.digest(), signature)
  }

  /**
   * A checksum does not have a distinct verify implementation
   */
  private trait ChecksumFactory extends SignerFactory {
    def newVerifier: Signer = newSigner
  }

  private class DigestFactory(val algorithm: String) extends ChecksumFactory {
    def newSigner: Signer = new DigestSigner(MessageDigest.getInstance(algorithm))
  }

  /**
   * http://en.wikipedia.org/wiki/Fletcher's_checksum
   */
  private class Fletcher16Checksum extends Signer {
    var checksum = (0, 0)
    def update(data: Array[Byte]): Unit = {
      checksum = data.foldLeft(checksum) { (p, b) =>
        val lsb = (p._2 + (0xff & b)) % 255
        ((p._1 + lsb) % 255, lsb)
      }
    }
    def sign: Array[Byte] = Array(checksum._1.asInstanceOf[Byte], checksum._2.asInstanceOf[Byte])
    def verify(signature: Array[Byte]): Boolean = Arrays.equals(sign, signature)
  }

  /**
   * `java.util.zip.Checksum` implementation of Signer
   */
  private class ZipChecksumSigner(impl: Checksum) extends Signer {
    def update(data: Array[Byte]): Unit = impl.update(data, 0, data.length)
    def sign: Array[Byte] = ByteVector.fromLong(impl.getValue()).drop(4).toArray
    def verify(signature: Array[Byte]): Boolean = MessageDigest.isEqual(sign, signature)
  }

} 