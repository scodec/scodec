package scodec

import scalaz.{ \/, StateT }

import scodec.bits.BitVector

/** Provides constructors for `DecodingContext`. */
object DecodingContext {

  /** Lifts a function of the shape `BitVector => Err \/ (BitVector, A)` to a decoding context. */
  def apply[A](f: BitVector => Err \/ (BitVector, A)): DecodingContext[A] =
    StateT[({type λ[+a] = Err \/ a})#λ, BitVector, A](f)

  /** Lifts a value of `Err \/ A` in to a decoding context. */
  def liftE[A](e: Err \/ A): DecodingContext[A] =
    apply { bv => e map { a => (bv, a) } }

  /** Provides a `MonadState` instance for `DecodingContext`. */
  def monadState = StateT.stateTMonadState[BitVector, ({type λ[+a] = Err \/ a})#λ]
}


