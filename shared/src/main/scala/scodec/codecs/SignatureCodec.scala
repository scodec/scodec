/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec
package codecs

import java.security.{KeyPair, PrivateKey, PublicKey, Signature, SignatureException}
import java.security.cert.Certificate

import scodec.bits.{BitVector, ByteVector}

/**
  * Represents the ability to create a "checksum" for use with [[fixedSizeSignature]] and [[variableSizeSignature]].
  */
trait Signer:
  def update(data: Array[Byte]): Unit
  def sign: Array[Byte]
  def verify(signature: Array[Byte]): Boolean

/**
  * Signer implementation for `java.security.Signature`
  */
class SignatureSigner(impl: Signature) extends Signer:
  def update(data: Array[Byte]): Unit = impl.update(data)
  def sign: Array[Byte] = impl.sign.nn
  def verify(signature: Array[Byte]): Boolean = impl.verify(signature)

/**
  * Represents the ability to create a [[Signer]] for use with [[fixedSizeSignature]] and [[variableSizeSignature]].
  */
trait SignerFactory:

  /** Creates a [[Signer]] initialized for signing. */
  def newSigner: Signer

  /** Creates a [[Signer]] initialized for verifying. */
  def newVerifier: Signer

/**
  * Create `java.security.Signature` implementations for [[SignerFactory]]
  */
object SignatureFactory:

  /** Creates a signature factory for the specified algorithm using the specified private and public keys. */
  def apply(algorithm: String, privateKey: PrivateKey, publicKey: PublicKey): SignerFactory =
    new SimpleSignatureFactory(algorithm, privateKey, publicKey)

  /** Creates a signature factory for the specified algorithm using the specified key pair. */
  def apply(algorithm: String, keyPair: KeyPair): SignerFactory =
    new SimpleSignatureFactory(algorithm, keyPair.getPrivate.nn, keyPair.getPublic.nn)

  /** Creates a signature factory for the specified algorithm using the specified key pair. */
  def apply(algorithm: String, privateKey: PrivateKey, certificate: Certificate): SignerFactory =
    new SimpleSignatureFactory(algorithm, privateKey, certificate.getPublicKey.nn)

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified private key. */
  def signing(algorithm: String, privateKey: PrivateKey): SignerFactory =
    new SimpleSignatureFactorySigning(algorithm, privateKey) with SignerFactory {
      def newVerifier =
        sys.error("Cannot verify with a signature factory that only supports signing.")
    }

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified public key. */
  def verifying(algorithm: String, publicKey: PublicKey): SignerFactory =
    new SimpleSignatureFactoryVerifying(algorithm, publicKey) with SignerFactory {
      def newSigner =
        sys.error("Cannot sign with a signature factory that only supports verifying.")
    }

  /** Creates a signature factory that only supports signing for the specified algorithm using the specified public key. */
  def verifying(algorithm: String, certificate: Certificate): SignerFactory =
    verifying(algorithm, certificate.getPublicKey.nn)

  private trait WithSignature:
    protected def algorithm: String
    protected def newSignature: Signature =
      Signature.getInstance(algorithm).nn

  private trait SignatureFactorySigning extends WithSignature:
    protected def privateKey: PrivateKey
    def newSigner: Signer =
      val sig = newSignature
      sig.initSign(privateKey)
      new SignatureSigner(sig)

  private class SimpleSignatureFactorySigning(
      protected val algorithm: String,
      protected val privateKey: PrivateKey
  ) extends SignatureFactorySigning

  private trait SignatureFactoryVerifying extends WithSignature:
    protected def publicKey: PublicKey
    def newVerifier: Signer =
      val sig = newSignature
      sig.initVerify(publicKey)
      new SignatureSigner(sig)

  private class SimpleSignatureFactoryVerifying(
      protected val algorithm: String,
      protected val publicKey: PublicKey
  ) extends SignatureFactoryVerifying

  private class SimpleSignatureFactory(
      protected val algorithm: String,
      protected val privateKey: PrivateKey,
      protected val publicKey: PublicKey
  ) extends SignerFactory
      with SignatureFactorySigning
      with SignatureFactoryVerifying


/** @see [[fixedSizeSignature]] and [[variableSizeSignature]] */
private[codecs] final class SignatureCodec[A](codec: Codec[A], signatureCodec: Codec[BitVector],
    signerFactory: SignerFactory
) extends Codec[A]:

  override def sizeBound = codec.sizeBound + signatureCodec.sizeBound

  override def encode(a: A) =
    for
      encoded <- codec.encode(a)
      sig <- sign(encoded)
      encodedSig <- signatureCodec.encode(sig)
    yield encoded ++ encodedSig

  private def sign(bits: BitVector): Attempt[BitVector] =
    try
      val signature = signerFactory.newSigner
      signature.update(bits.toByteArray)
      Attempt.successful(BitVector(signature.sign))
    catch
      case e: SignatureException =>
        Attempt.failure(Err("Failed to sign: " + e.getMessage))

  override def decode(buffer: BitVector) =
    (for
      initialBits <- Decoder.get
      value <- codec
      bitsAfterValueDecoding <- Decoder.get
      valueBits = initialBits.take(initialBits.size - bitsAfterValueDecoding.size)
      decodedSig <- signatureCodec
      _ <- Decoder.liftAttempt(verify(valueBits.toByteVector, decodedSig.toByteVector))
    yield value).decode(buffer)

  private def verify(data: ByteVector, signatureBytes: ByteVector): Attempt[Unit] =
    val verifier = signerFactory.newVerifier
    verifier.update(data.toArray)
    try
      if verifier.verify(signatureBytes.toArray) then
        Attempt.successful(())
      else
        Attempt.failure(Err("Signature verification failed"))
    catch
      case e: SignatureException =>
        Attempt.failure(Err("Signature verification failed: " + e))

  override def toString = s"signature($codec, $signatureCodec)"
