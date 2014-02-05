package scodec

import scalaz.{ \/, StateT }

/** Provides constructors for `DecodingContext`. */
object DecodingContext {

  /** Lifts a function of the shape `BitVector => Error \/ (BitVector, A)` to a decoding context. */
  def apply[A](f: BitVector => Error \/ (BitVector, A)): DecodingContext[A] =
    StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A](f)

  /** Lifts a value of `Error \/ A` in to a decoding context. */
  def liftE[A](e: Error \/ A): DecodingContext[A] =
    apply { bv => e map { a => (bv, a) } }

  /** Provides a `MonadState` instance for `DecodingContext`. */
  def monadState = StateT.stateTMonadState[BitVector, ({type λ[+a] = Error \/ a})#λ]
}


