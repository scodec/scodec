package scodec

import scala.language.implicitConversions

/**
 * Type class that supports implicit lookup of implicit `Codec` instances with a fallback to automatically derived codecs.
 *
 * When writing general combinators that depend on implicit codecs, it is generally best to use `ImplicitCodec[A]` as
 * an implicit parameter, as opposed to using `Codec[A]` directly. The advantage of using `ImplicitCodec[A]` is that
 * it allows for codecs that can be automatically derived via `Codec.derive`.
 *
 * @groupname primary Primary Members
 * @groupprio primary 0
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 10
 *
 * @groupname tuple Tuple Support
 * @groupprio tuple 11
 *
 * @groupname hlist HList Support
 * @groupprio hlist 12
 *
 * @groupname coproduct Coproduct Support
 * @groupprio coproduct 13
 */
@annotation.implicitNotFound("""Could not find an implicit Codec[${A}] and a codec could not be automatically derived. Derivation support exists for case classes, where each component type has an implicit codec in scope, and for sealed class hierarchies, where there is an implicit Discriminated[${A}, D] and implicit Discriminators[${A}, X, D] for each subtype X of ${A}.""")
sealed abstract class ImplicitCodec[A] {
  def codec: Codec[A]
}

/** Defines derived codecs as a fallback to regular implicit `Codec` instances. */
sealed trait LowPriorityImplicitCodec {
  /** Lifts a derived codec in to an implicit codec. */
  implicit def derive[A](implicit d: DerivedCodec[A]): ImplicitCodec[A] = wrap(d.codec)

  /** Lifts a codec in to an implicit codec. */
  def wrap[A](c: Codec[A]): ImplicitCodec[A] = new ImplicitCodec[A] {
    val codec = c
    override def toString = c.toString
  }
}

/** Companion for [[ImplicitCodec]]. */
object ImplicitCodec extends LowPriorityImplicitCodec {

  /** Implicitly unwraps an implicit codec to a regular codec. */
  implicit def unwrap[A](ic: ImplicitCodec[A]): Codec[A] = ic.codec

  /** Implicitly wraps an implicit codec instance. */
  implicit def implicitWrap[A](implicit c: Codec[A]): ImplicitCodec[A] = wrap(c)

  /** Gets the implicitly available codec for type `A` -- either an explicitly defined implicit or a derived codec. */
  def apply[A](implicit ic: ImplicitCodec[A]): ImplicitCodec[A] = ic
}
