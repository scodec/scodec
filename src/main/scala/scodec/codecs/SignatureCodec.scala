package scodec
package codecs

import java.security.{KeyPair, PrivateKey, PublicKey, Signature, SignatureException}
import java.security.cert.Certificate

import scodec.bits.{ BitVector, ByteVector }

/**
 * Represents the ability to create a "checksum" for use with [[fixedSizeSignature]] and [[variableSizeSignature]].
 * @group crypto
 */
trait Signer {
   def update(data:Array[Byte]):Unit
   def sign:Array[Byte]
   def verify(signature:Array[Byte]):Boolean
}

/**
 * Signer implementation for `java.security.Signature`
 * @group crypto
 */
class SignatureSigner(impl:Signature) extends Signer {
   def update(data:Array[Byte]):Unit = impl.update(data)
   def sign:Array[Byte] = impl.sign
   def verify(signature:Array[Byte]):Boolean = impl.verify(signature)
}

/**
 * Represents the ability to create a [[Signer]] for use with [[fixedSizeSignature]] and [[variableSizeSignature]].
 * @group crypto
 */
trait SignerFactory {

  /** Creates a [[Signer]] initialized for signing. */
  def newSigner: Signer

  /** Creates a [[Signer]] initialized for verifying. */
  def newVerifier: Signer
}

/**
 * Create `java.security.Signature` implementations for [[SignerFactory]]
 * @group crypto
 */
object SignatureFactory {

  /** Creates a signature factory for the specified algorithm using the specified private and public keys. */
  def apply(algorithm: String, privateKey: PrivateKey, publicKey: PublicKey): SignerFactory =
    new SimpleSignatureFactory(algorithm, privateKey, publicKey)

  /** Creates a signature factory for the specified algorithm using the specified key pair. */
  def apply(algorithm: String, keyPair: KeyPair): SignerFactory =
    new SimpleSignatureFactory(algorithm, keyPair.getPrivate, keyPair.getPublic)

  /** Creates a signature factory for the specified algorithm using the specified key pair. */
  def apply(algorithm: String, privateKey: PrivateKey, certificate: Certificate): SignerFactory =
    new SimpleSignatureFactory(algorithm, privateKey, certificate.getPublicKey)

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified private key. */
  def signing(algorithm: String, privateKey: PrivateKey): SignerFactory =
    new SimpleSignatureFactorySigning(algorithm, privateKey) with SignerFactory {
      def newVerifier = sys.error("Cannot verify with a signature factory that only supports signing.")
    }

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified public key. */
  def verifying(algorithm: String, publicKey: PublicKey): SignerFactory =
    new SimpleSignatureFactoryVerifying(algorithm, publicKey) with SignerFactory {
      def newSigner = sys.error("Cannot sign with a signature factory that only supports verifying.")
    }

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified public key. */
  def verifying(algorithm: String, certificate: Certificate): SignerFactory =
    verifying(algorithm, certificate.getPublicKey)


  private trait WithSignature {
    protected def algorithm: String
    protected def newSignature: Signature =
      Signature.getInstance(algorithm)
  }

  private trait SignatureFactorySigning extends WithSignature {
    protected def privateKey: PrivateKey
    def newSigner: Signer = {
      val sig = newSignature
      sig.initSign(privateKey)
      new SignatureSigner(sig)
    }
  }

  private class SimpleSignatureFactorySigning(
    protected val algorithm: String,
    protected val privateKey: PrivateKey
  ) extends SignatureFactorySigning

  private trait SignatureFactoryVerifying extends WithSignature {
    protected def publicKey: PublicKey
    def newVerifier: Signer = {
     val sig = newSignature
      sig.initVerify(publicKey)
      new SignatureSigner(sig)
    }
  }

  private class SimpleSignatureFactoryVerifying(
    protected val algorithm: String,
    protected val publicKey: PublicKey
  ) extends SignatureFactoryVerifying

  private class SimpleSignatureFactory(
    protected val algorithm: String,
    protected val privateKey: PrivateKey,
    protected val publicKey: PublicKey
  ) extends SignerFactory with SignatureFactorySigning with SignatureFactoryVerifying

}

/** @see [[fixedSizeSignature]] and [[variableSizeSignature]] */
private[codecs] final class SignatureCodec[A](codec: Codec[A], signatureCodec: Codec[BitVector])(implicit signerFactory: SignerFactory) extends Codec[A] {
  import Codec._

  override def encode(a: A) = for {
    encoded <- codec.encode(a)
    sig <- sign(encoded)
    encodedSig <- signatureCodec.encode(sig)
  } yield encoded ++ encodedSig

  private def sign(bits: BitVector): Attempt[BitVector] = {
    try {
      val signature = signerFactory.newSigner
      signature.update(bits.toByteArray)
      Attempt.successful(BitVector(signature.sign))
    } catch {
      case e: SignatureException =>
        Attempt.failure(Err("Failed to sign: " + e.getMessage))
    }
  }

  override def decode(buffer: BitVector) = (for {
    initialBits <- DecodingContext.get
    value <- DecodingContext(codec)
    bitsAfterValueDecoding <- DecodingContext.get
    valueBits = initialBits take (initialBits.size - bitsAfterValueDecoding.size)
    decodedSig <- DecodingContext(signatureCodec)
    _ <- DecodingContext liftAttempt verify(valueBits.toByteVector, decodedSig.toByteVector)
  } yield value).decode(buffer)

  private def verify(data: ByteVector, signatureBytes: ByteVector): Attempt[Unit] = {
    val verifier = signerFactory.newVerifier
    verifier.update(data.toArray)
    try {
      if (verifier.verify(signatureBytes.toArray)) {
        Attempt.successful(())
      } else {
        Attempt.failure(Err("Signature verification failed"))
      }
    } catch {
      case e: SignatureException =>
        Attempt.failure(Err("Signature verification failed: " + e))
    }
  }

  override def toString = s"signature($codec, $signatureCodec)"
}
