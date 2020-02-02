// package scodec
// package codecs

// import shapeless._

// /** Provides syntax support for `Codec[L]#derive` for some `HList L`.  See `derive` for more information. */
// class DeriveHListElementAux[L <: HList, A](val codec: Codec[L]) extends AnyVal

// /** Companion for [[DeriveHListElementAux]]. */
// object DeriveHListElementAux {

//   /**
//     * Builds a `Codec[M]` using the supplied function `M => A`. The returned codec uses the original
//     * `Codec[L]` for encoding and decoding. When encoding, it computes an `A` value using the supplied
//     * function and inserts the computed `A` in to the `HList M`, yielding an `HList L`. That `HList L`
//     * is then encoded using the original codec.
//     */
//   implicit class From[L <: HList, A, M <: HList](val aux: DeriveHListElementAux[L, A])(
//       implicit remove: InvertibleRemove.Aux[L, A, M]
//   ) {
//     def from(f: M => A): Codec[M] =
//       aux.codec.xmap[M](remove(_), m => remove.invert(m, f(m)))
//   }
// }
