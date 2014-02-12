package scodec
package codecs

import scalaz.{ \/, IndexedStateT }
import scalaz.syntax.id._
import scalaz.syntax.std.option._

import scodec.bits.BitVector

/**
 * Codec that supports encoding/decoding some values of type `A` by including a value discriminator in the binary encoding.
 * The binary encoding is the encoded discriminator value followed by the encoded value.
 *
 * Enconding is performed by:
 *  - determining the discriminator for the value using `discriminator.discriminate`
 *  - determining the codec for the value by passing the discriminator value to `discriminator.codec`
 *  - encoding the discriminator using the `disciminatorCodec`
 *  - encoding the value using the looked up codec
 *
 * Decoding is performed by:
 *  - decoding a discriminator value using the `discriminatorCodec`
 *  - looking up the value codec by passing the discriminator value to `discriminator.codec`
 *  - decoding the value
 */
private[codecs] final class DiscriminatorCodec[A, B](discriminator: Discriminator[A, B], discriminatorCodec: Codec[B]) extends Codec[A] {

  override def encode(value: A) = {
    for {
      discriminatorValue <- discriminator.discriminate(value) \/> s"No discriminator defined for value '$value'"
      valueCodec <- discriminator.codec(discriminatorValue) \/> s"No codec defined for discriminator '$discriminator' for value '$value'"
      result <- Codec.encodeBoth(discriminatorCodec, valueCodec)(discriminatorValue, value)
    } yield result
  }

  override def decode(buffer: BitVector) = {
    for {
      discriminatorValue <- DecodingContext(discriminatorCodec.decode)
      valueCodec <- IndexedStateT.StateMonadTrans.liftM[({type λ[+α] = String \/ α})#λ, Codec[A]](
        discriminator.codec(discriminatorValue) \/> s"No codec defined for discriminator '$discriminator'"
      )
      value <- DecodingContext(valueCodec.decode)
    } yield value
  }.run(buffer)

  override def toString = s"discriminated($discriminatorCodec)"
}

/** Companion for [[Discriminator]]. */
private[codecs] object DiscriminatorCodec {
  def apply[A, B](discriminatorCodec: Codec[B], discriminate: PartialFunction[A, B], codecFor: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B] =
    new DiscriminatorCodec(Discriminator[A, B](discriminate, codecFor), discriminatorCodec)
}

