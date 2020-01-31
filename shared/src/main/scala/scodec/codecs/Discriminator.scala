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
