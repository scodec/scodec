package scodec
package codecs

/**
 * Wrapper class that discriminates type `Y` in context of type `X` with a
 * value of type `B`.
 *
 * For example, consider a sealed trait and its subclasses. Each subclass
 * could be assigned an integer by defining a `Discriminator[Parent, Subclass, Int]`
 * for each.
 */
case class Discriminator[X, Y, D](value: D)

/**
 * Wrapper class that indicates subtypes of `Y` are discriminated by type `D`
 * using the supplied `Codec[D]`.
 *
 * For example, an implicit `Discriminated` value can be defined in the companion
 * of a sealed trait, along with implicit `Discriminator` values in each subtype
 * companion. Given such implicits, a codec for the trait can be automatically
 * created using `Codec.derive[Y]`.
 */
case class Discriminated[Y, D](codec: Codec[D])
