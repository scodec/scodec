package scodec

import scalaz.{\/, IndexedStateT}
import scalaz.syntax.id._
import scalaz.syntax.std.option._

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
class DiscriminatorCodec[A, B](discriminator: Discriminator[A, B], discriminatorCodec: Codec[B]) extends Codec[A] {

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
      valueCodec <- IndexedStateT.StateMonadTrans.liftM[({type λ[+α] = Error \/ α})#λ, Codec[A]](
        discriminator.codec(discriminatorValue) \/> s"No codec defined for discriminator '$discriminator'"
      )
      value <- DecodingContext(valueCodec.decode)
    } yield value
  }.run(buffer)

  override def toString = s"discriminated($discriminatorCodec)"
}

/** Companion for [[Discriminator]]. */
object DiscriminatorCodec {
  def apply[A, B](discriminatorCodec: Codec[B], discriminate: PartialFunction[A, B], codecFor: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B] =
    new DiscriminatorCodec(Discriminator[A, B](discriminate, codecFor), discriminatorCodec)
}

/** Companion for [[DiscriminatorCodecSyntax]]. */
object DiscriminatorCodecSyntax {

  sealed trait NeedDiscriminatorCodec[A] {
    def by[B](discriminatorCodec: Codec[B]): NeedDiscriminator[A, B]
  }

  sealed trait NeedDiscriminator[A, B] {
    def using(discriminator: Discriminator[A, B]): DiscriminatorCodec[A, B]
    def using(discriminate: PartialFunction[A, B], codec: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B]
  }
}

/** Provides syntax for constructing a [[DiscriminatorCodec]] and related types. */
private[scodec] trait DiscriminatorCodecSyntax extends TypeDiscriminatorSyntax {
  import DiscriminatorCodecSyntax._

  /**
   * Provides syntax for building a [[DiscriminatorCodec]].
   * Usage: {{{
   val discriminator: Discriminator[AnyVal, Int] = ???
   val codec = discriminated[AnyVal] by uint8 using discriminator
   }}}
   */
  final def discriminated[A]: NeedDiscriminatorCodec[A] = new NeedDiscriminatorCodec[A] {
    final def by[B](discriminatorCodec: Codec[B]): NeedDiscriminator[A, B] = new NeedDiscriminator[A, B] {
      final def using(discriminator: Discriminator[A, B]): DiscriminatorCodec[A, B] =
        new DiscriminatorCodec(discriminator, discriminatorCodec)
      final def using(discriminate: PartialFunction[A, B], codec: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B] =
        new DiscriminatorCodec(Discriminator[A, B](discriminate, codec), discriminatorCodec)
    }
  }
}
