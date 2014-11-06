package scodec

import scala.language.implicitConversions
import shapeless._
import shapeless.record._
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.syntax._

/**
 * Wrapper for a codec that was automatically derived by compile time reflection of the structure of the type.
 *
 * This type is not typically used directly. Rather, to get a derived codec,
 * call `Codec.derive[A]`. This type is an implementation detail of `Codec.derive`.
 * See the docs for [[Codec.derive]] for more information.
 *
 * Instances of this type can be used directly as a `Codec[A]`. There is no need to manually unwrap by calling `d.codec`.
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
@annotation.implicitNotFound("""Could not derive a codec automatically for ${A}. Support exists for case classes, where each component type has an implicit codec in scope, and for sealed class hierarchies, where there is an implicit Discriminated[${A}, D] and implicit Discriminators[${A}, X, D] for each subtype X of ${A}.""")
sealed abstract class DerivedCodec[A] {
  def codec: Codec[A]
}

/** Companion for [[DerivedCodec]]. */
object DerivedCodec {

  /** Implicitly unwraps an implicit codec to a regular codec. */
  implicit def unwrap[A](dc: DerivedCodec[A]): Codec[A] = dc.codec

  /** Lifts a codec in to a derived codec. */
  private def wrap[A](c: Codec[A]): DerivedCodec[A] = new DerivedCodec[A] {
    val codec = c
    override def toString = c.toString
  }

  /** Derives a codec for the specified type. */
  def apply[A](implicit dc: DerivedCodec[A]): DerivedCodec[A] = dc

  implicit def hnil: DerivedCodec[HNil] =
    wrap(codecs.HListCodec.hnilCodec)

  implicit def hlist[H, T <: HList](implicit headCodec: ImplicitCodec[H], tailAux: DerivedCodec[T]): DerivedCodec[H :: T] =
    wrap(headCodec :: tailAux.codec)

  implicit def record[KH <: Symbol, VH, TRec <: HList, KT <: HList](implicit
    keys: Keys.Aux[FieldType[KH, VH] :: TRec, KH :: KT],
    headCodec: ImplicitCodec[VH],
    tailAux: DerivedCodec[TRec]
  ): DerivedCodec[FieldType[KH, VH] :: TRec] = wrap {
    val namedHeadCodec: Codec[VH] = headCodec withContext keys().head.name
    val headFieldCodec: Codec[FieldType[KH, VH]] = namedHeadCodec.toField[KH]
    headFieldCodec :: tailAux.codec
  }

  implicit def labelledProduct[A, Rec <: HList](implicit
    lgen: LabelledGeneric.Aux[A, Rec],
    auto: DerivedCodec[Rec]
  ): DerivedCodec[A] = wrap(auto.codec.xmap(lgen.from, lgen.to))

  implicit def coproduct[A, D, C0 <: Coproduct](implicit
    discriminated: codecs.Discriminated[A, D],
    auto: codecs.CoproductBuilderAuto[A] { type C = C0 },
    auto2: codecs.CoproductBuilderAutoDiscriminators[A, C0, D]
  ): DerivedCodec[A] = wrap(auto.apply.auto)
}
