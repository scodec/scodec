package scodec

import scala.collection.immutable.IndexedSeq

/**
 * Provides support for building codecs that encode/decode multiple disparate types by using a discriminator value to
 * indicate the type.
 */
sealed trait Discriminator[A, B] {

  /** Gets the discriminator for the specified value. */
  def discriminate(a: A): Option[B]

  /** Gets the codec for encoding/decoding the values that map to the specified discriminator. */
  def codec(b: B): Option[Codec[A]]
}

/** Companion for [[Discriminator]]. */
object Discriminator {

  /** Creates a discriminator from partial functions. */
  def apply[A, B](discriminate: PartialFunction[A, B], codec: PartialFunction[B, Codec[_ <: A]]): Discriminator[A, B] =
    apply[A, B](discriminate.lift, codec.lift)

  /** Creates a discriminator from functions. */
  def apply[A, B](discriminate: A => Option[B], codec: B => Option[Codec[_ <: A]]): Discriminator[A, B] = {
    val d = discriminate
    val c = codec
    new Discriminator[A, B] {
      def discriminate(a: A) = d(a)
      def codec(b: B) = c(b).asInstanceOf[Option[Codec[A]]]
    }
  }
}

/**
 * A case in a type based discriminator.
 *
 * @tparam A type this case is for
 * @tparam B type of discriminator
 * @param discriminatorValue discriminator value mapped to type `A`
 * @param codec codec to use for encoding/decoding values of type `A`
 * @see [[TypeDiscriminator]]
 */
case class TypeDiscriminatorCase[A: Manifest, B](discriminatorValue: B, codec: Codec[A]) {
  /** Gets the manifest of the type this case is for. */
  def manifest: Manifest[A] = implicitly[Manifest[A]]
}

/**
 * Discriminator that matches based on the runtime type of the value.
 *
 * To construct a type discriminator, import `Codecs._` and do the following:
 {{{
  typeDiscriminator[Any, Int](
    typeDiscriminatorCase(0, bool)
    typeDiscriminatorCase(1, int32)
    typeDiscriminatorCase(2, ascii)
  )
 }}}
 */
final class TypeDiscriminator[A, B](cases: IndexedSeq[TypeDiscriminatorCase[_ <: A, B]]) extends Discriminator[A, B] {

  def discriminate(a: A) = (cases find matchesClass(a)).map { _.discriminatorValue }

  private def matchesClass(a: A)(c: TypeDiscriminatorCase[_, _]) = {
    val clazz = c.manifest match {
      case Manifest.Byte => classOf[java.lang.Byte]
      case Manifest.Char => classOf[java.lang.Character]
      case Manifest.Short => classOf[java.lang.Short]
      case Manifest.Int => classOf[java.lang.Integer]
      case Manifest.Long => classOf[java.lang.Long]
      case Manifest.Float => classOf[java.lang.Float]
      case Manifest.Double => classOf[java.lang.Double]
      case Manifest.Boolean => classOf[java.lang.Boolean]
      case m => m.runtimeClass
    }
    clazz.isAssignableFrom(a.getClass)
  }

  def codec(b: B) = discriminators get b

  private val discriminators = cases.map { c => c.discriminatorValue -> c.codec.asInstanceOf[Codec[A]] }.toMap
}

private[scodec] trait TypeDiscriminatorSyntax {

  final def typeDiscriminator[A, B](cases: TypeDiscriminatorCase[_ <: A, B]*): Discriminator[A, B] =
    new TypeDiscriminator[A, B](cases.toIndexedSeq)

  final def typeDiscriminatorCase[A: Manifest, B](discriminatorValue: B, codec: Codec[A]): TypeDiscriminatorCase[A, B] =
    TypeDiscriminatorCase(discriminatorValue, codec)
}

/** Companion for [[TypeDiscriminator]]. */
object TypeDiscriminator extends TypeDiscriminatorSyntax
