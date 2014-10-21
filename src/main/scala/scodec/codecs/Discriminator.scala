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
