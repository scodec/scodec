package scodec
package codecs

import shapeless.=:!=

/**
  * Wrapper class that indicates subtypes of `X` are discriminated by type `D`
  * using the supplied `Codec[D]`.
  *
  * For example, an implicit `Discriminated` value can be defined in the companion
  * of a sealed trait, along with implicit `Discriminator` values in each subtype
  * companion. Given such implicits, a codec for the trait can be automatically
  * derived using `Codec[X]`.
  *
  * Contains an optional codec transformation, which is applied to every
  * component codec before encoding/decoding. This allows common structure,
  * e.g., size based framing, to be specified in one location.
  *
  * For example, if each discriminated subtype is prefixed by a size field,
  * the framing could be specified as: {{{
  Discriminated[X, D](uint, new CodecTransformation {
    def apply[Z](c: Codec[Z]) = variableSizeBytes(uint16, c)
  })
 }}}
  *
  * @tparam X type that can be discriminated by discriminator values of type `D`
  * @tparam D value type that discriminates `Y` in context of `X` from other types
  *           using discriminators of `D` for some type `Y`
  */
final case class Discriminated[X, D](codec: Codec[D], framing: CodecTransformation) {
  def this(codec: Codec[D]) = this(codec, CodecTransformation.Id)

  /** Binds the discriminator value `D` to type `Y`. */
  def bind[Y <: X](discriminator: D)(implicit ev: Y =:!= X): Discriminator[X, Y, D] = {
    val _ = ev // Convince scalac ev is used
    Discriminator(discriminator)
  }
}

/** Companion for [[Discriminated]]. */
object Discriminated {
  def apply[X, D](codec: Codec[D]): Discriminated[X, D] = new Discriminated(codec)
}
