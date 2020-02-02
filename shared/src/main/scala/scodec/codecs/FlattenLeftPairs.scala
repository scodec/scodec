// package scodec
// package codecs

// import shapeless._
// import ops.hlist.Reverse

// /**
//   * Type level operation on type `A` which flattens left unflattened pairs in to an hlist and vice versa.
//   *
//   * That is, the `flatten` method converts a `(((A, B), C), D)` in to a `A :: B :: C :: D :: HNil` and
//   * the `unflatten` method does the inverse.
//   */
// sealed trait FlattenLeftPairs[A] extends DepFn1[A] {
//   type Out <: HList
//   final def apply(in: A): Out = flatten(in)
//   def flatten(in: A): Out
//   def unflatten(in: Out): A
// }

// /** Companion for [[FlattenLeftPairs]]. */
// object FlattenLeftPairs {
//   type Aux[A0, Out0] = FlattenLeftPairs[A0] { type Out = Out0 }

//   /** Builds an [[FlattenLeftPairs]] instance using a builder. */
//   implicit def mk[A, K <: HList, L <: HList](
//       implicit
//       builder: Builder.Aux[A, K],
//       reverseK: Reverse.Aux[K, L],
//       reverseL: Reverse.Aux[L, K]
//   ): FlattenLeftPairs.Aux[A, L] = new FlattenLeftPairs[A] {
//     type Out = L
//     def flatten(in: A): L = reverseK(builder.flatten(in))
//     def unflatten(in: L): A = builder.unflatten(reverseL(in))
//   }

//   /** Similar to [[FlattenLeftPairs]] but the output list is in reverse order. */
//   sealed trait Builder[A] {
//     type Out <: HList
//     def flatten(in: A): Out
//     def unflatten(in: Out): A
//   }

//   sealed trait BuilderLowPriority {
//     implicit def nonTuple[A, B]: Builder.Aux[A, A :: HNil] = new Builder[A] {
//       type Out = A :: HNil
//       def flatten(in: A) = in :: HNil
//       def unflatten(in: A :: HNil) = in.head
//     }
//   }

//   object Builder extends BuilderLowPriority {
//     type Aux[A0, Out0] = Builder[A0] { type Out = Out0 }

//     implicit def left[A, B, AL <: HList](
//         implicit
//         left: Builder.Aux[A, AL]
//     ): Builder.Aux[(A, B), B :: AL] = new Builder[(A, B)] {
//       type Out = B :: AL
//       def flatten(in: (A, B)) = in._2 :: left.flatten(in._1)
//       def unflatten(in: B :: AL) = (left.unflatten(in.tail), in.head)
//     }
//   }
// }
