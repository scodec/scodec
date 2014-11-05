package scodec

import scala.language.implicitConversions

/**
 * Type class that supports implicit lookup of implicit `Codec` instances with a fallback to automatically derived codecs.
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
