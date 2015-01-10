package scodec
package codecs

/**
 * Wrapper class that discriminates type `Y` in context of type `X` with a
 * value of type `D`.
 *
 * For example, consider a sealed trait and its subclasses. Each subclass
 * could be assigned an integer by defining a `Discriminator[Parent, Subclass, Int]`
 * for each.
 *
 * @tparam X context in which type `Y` is discriminated from other types
 * @tparam Y type that is differentiated from other types by values of `D`
 * @tparam D value type that discriminates values of `Y` from other types in context of `X`
 */
final case class Discriminator[X, Y, D](value: D)

/**
 * Wrapper class that indicates subtypes of `X` are discriminated by type `D`
 * using the supplied `Codec[D]`.
 *
 * For example, an implicit `Discriminated` value can be defined in the companion
 * of a sealed trait, along with implicit `Discriminator` values in each subtype
 * companion. Given such implicits, a codec for the trait can be automatically
 * derived using `Codec[X]`.
 *
 * @tparam X type that can be discriminated by discriminator values of type `D`
 * @tparam D value type that discriminates `Y` in context of `X` from other types
 *           using discriminators of `D` for some type `Y`
 */
final case class Discriminated[X, D](codec: Codec[D])
