package scodec

import scalaz.{\/, IndexedStateT}
import scalaz.syntax.id._
import scalaz.syntax.std.option._

import Codec.DecodingContext

/**
 * Codec that supports encoding/decoding some values of type `A` by including a value discriminator in the binary encoding.
 * The binary encoding is the encoded discriminator followed by the encoded value.
 *
 * Enconding is performed by:
 *  - determining the discriminator for the value using `discriminate`
 *  - determining the codec for the value by passing the discriminator to `codecFor`
 *  - encoding the discriminator using the `disciminatorCodec`
 *  - encoding the value using the looked up codec
 *
 * Decoding is performed by:
 *  - decoding a discriminator value using the `discriminatorCodec`
 *  - looking up the value codec by passing the discriminator to `codecFor`
 *  - decoding the value
 */
abstract class DiscriminatorCodec[A, B] extends Codec[A] {

  protected def discriminate: PartialFunction[A, B]
  protected def discriminatorCodec: Codec[B]
  protected def codecFor: PartialFunction[B, Codec[_ <: A]]

  override def encode(value: A) = {
    val discriminator = discriminate(value)
    for {
      valueCodec <- codecFor.lift(discriminator) \/> s"No codec defined for discriminator '$discriminator' for value '$value'"
      result <- Codec.encodeBoth(discriminatorCodec, valueCodec.asInstanceOf[Codec[A]])(discriminator, value)
    } yield result
  }

  override def decode(buffer: BitVector) = {
    for {
      discriminator <- DecodingContext(discriminatorCodec.decode)
      valueCodec <- IndexedStateT.StateMonadTrans.liftM[({type λ[+α] = Error \/ α})#λ, Codec[A]](
        codecFor.lift(discriminator).asInstanceOf[Option[Codec[A]]] \/> s"No codec defined for discriminator '$discriminator'"
      )
      value <- DecodingContext(valueCodec.decode)
    } yield value
  }.run(buffer)

  override def toString = s"discriminated($discriminatorCodec)"
}

object DiscriminatorCodec {
  def apply[A, B](discriminatorCodec: Codec[B], discriminate: PartialFunction[A, B], codecFor: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B] = {
    val dc = discriminatorCodec
    val d = discriminate
    val cf = codecFor
    new DiscriminatorCodec[A, B] {
      val discriminatorCodec = dc
      val discriminate = d
      val codecFor = cf
    }
  }
}

object DiscriminatorCodecSyntax {

  sealed trait NeedDiscriminator[A] {
    def by[B](discriminatorCodec: Codec[B])(discriminate: PartialFunction[A, B]): NeedCodecs[A, B]
  }

  sealed trait NeedCodecs[A, B] {
    def withCodecs(codecFor: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B]
  }
}

private[scodec] trait DiscriminatorCodecSyntax {
  import DiscriminatorCodecSyntax._

  /** Provides syntax for building a [[DiscriminatorCodec]]. */
  final def discriminated[A]: NeedDiscriminator[A] = new NeedDiscriminator[A] {
    final def by[B](discriminatorCodec: Codec[B])(discriminate: PartialFunction[A, B]): NeedCodecs[A, B] = new NeedCodecs[A, B] {
      final def withCodecs(codecFor: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B] =
        DiscriminatorCodec[A, B](discriminatorCodec, discriminate, codecFor)
    }
  }
}
