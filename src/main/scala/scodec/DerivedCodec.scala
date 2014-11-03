package scodec

import scala.language.implicitConversions
import shapeless._
import shapeless.record._
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.syntax._

/**
 * Wrapper for a codec that was automatically derived.
 *
 * This type is not typically used directly. Rather, to get a derived codec,
 * call `Codec.derive[A]`. This type is typically used as an implicit witness parameter.
 *
 * Codecs can be derived for:
 *  - case classes, when each component type of the case has an implicitly available codec
 *  - sealed class hierarchies, where:
 *    - the root type, `A`, has an implicitly availble `Discriminated[A, D]` for some `D`
 *    - each subtype has an implicitly available codec or can have one derived
 *    - each subtype `X` has an implicitly available `Discriminator[A, X, D]`
 *
 * Instances of this type can be used directly as a `Codec[A]`. There is no need to manually unwrap by calling `d.codec`.
 */
@annotation.implicitNotFound("""Could not derive a codec automatically for ${A}. Support exists for case classes, where each component type has an implicit codec in scope, and for sealed class hierarchies, where there is an implicit Discriminated[${A}, D] and implicit Discriminators[${A}, X, D] for each subtype X of ${A}.""")
sealed abstract class DerivedCodec[A] {
  def codec: Codec[A]
}

/** Companion for [[DerivedCodec]]. */
object DerivedCodec {

  implicit def unwrap[A](dc: DerivedCodec[A]): Codec[A] = dc.codec

  /** Derives a codec for the specified type. */
  def apply[A](implicit dc: DerivedCodec[A]): DerivedCodec[A] = dc

  implicit def hnil: DerivedCodec[HNil] =
    new DerivedCodec[HNil] { val codec = codecs.HListCodec.hnilCodec }

  implicit def hlist[H, T <: HList](implicit headCodec: ImplicitCodec[H], tailAux: DerivedCodec[T]): DerivedCodec[H :: T] =
    new DerivedCodec[H :: T] { val codec = headCodec :: tailAux.codec }

  implicit def record[KH <: Symbol, VH, TRec <: HList, KT <: HList](implicit
    keys: Keys.Aux[FieldType[KH, VH] :: TRec, KH :: KT],
    headCodec: ImplicitCodec[VH],
    tailAux: DerivedCodec[TRec]
  ): DerivedCodec[FieldType[KH, VH] :: TRec] = new DerivedCodec[FieldType[KH, VH] :: TRec] {
    val codec = {
      import codecs.StringEnrichedWithCodecNamingSupport
      val namedHeadCodec: Codec[VH] = keys().head.name | headCodec
      val headFieldCodec: Codec[FieldType[KH, VH]] = namedHeadCodec.toField[KH]
      headFieldCodec :: tailAux.codec
    }
  }

  implicit def labelledProduct[A, Rec <: HList](implicit
    lgen: LabelledGeneric.Aux[A, Rec],
    auto: DerivedCodec[Rec]
  ): DerivedCodec[A] = new DerivedCodec[A] {
    val codec = auto.codec.xmap(lgen.from, lgen.to)
  }

  implicit def coproduct[A, D, C0 <: Coproduct](implicit
    discriminated: codecs.Discriminated[A, D],
    auto: codecs.CoproductBuilderAuto[A] { type C = C0 },
    auto2: codecs.CoproductBuilderAutoDiscriminators[A, C0, D]
  ): DerivedCodec[A] = new DerivedCodec[A] {
    def codec = auto.apply.auto
  }
}
