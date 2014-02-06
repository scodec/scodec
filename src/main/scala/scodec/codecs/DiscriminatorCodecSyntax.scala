package scodec
package codecs

object DiscriminatorCodecSyntax {

  sealed trait NeedDiscriminatorCodec[A] {
    def by[B](discriminatorCodec: Codec[B]): NeedDiscriminator[A, B]
  }

  sealed trait NeedDiscriminator[A, B] {
    def using(discriminator: Discriminator[A, B]): DiscriminatorCodec[A, B]
    def using(discriminate: PartialFunction[A, B], codec: PartialFunction[B, Codec[_ <: A]]): DiscriminatorCodec[A, B]
  }

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
