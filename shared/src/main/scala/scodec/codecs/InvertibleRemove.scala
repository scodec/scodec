// package scodec
// package codecs

// import shapeless._

// /** Similar to Shapeless's remove support for HLists but supports inverting the removal. */
// sealed trait InvertibleRemove[L <: HList, A] {
//   type Out <: HList
//   def apply(l: L): Out
//   def invert(out: Out, a: A): L
// }

// /** Companion for [[InvertibleRemove]]. */
// object InvertibleRemove {
//   type Aux[L0 <: HList, A0, Out0 <: HList] = InvertibleRemove[L0, A0] { type Out = Out0 }

//   implicit def forTargetType[T <: HList, A]: InvertibleRemove.Aux[A :: T, A, T] =
//     new InvertibleRemove[A :: T, A] {
//       type Out = T
//       def apply(in: A :: T): T = in.tail
//       def invert(out: T, a: A): A :: T = a :: out
//     }

//   implicit def forNonTargetType[H, T <: HList, A](
//       implicit rest: InvertibleRemove[T, A]
//   ): InvertibleRemove.Aux[H :: T, A, H :: rest.Out] =
//     new InvertibleRemove[H :: T, A] {
//       type Out = H :: rest.Out
//       def apply(l: H :: T): Out =
//         l.head :: rest(l.tail)
//       def invert(out: H :: rest.Out, a: A): H :: T =
//         out.head :: rest.invert(out.tail, a)
//     }
// }
