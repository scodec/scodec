import scalaz.{ \/, Monoid, StateT }

/**
 * Combinator library for working with binary data.
  */
package object scodec {

  type Error = String

  /** Alias for state/either transformer that simplifies calling decode on a series of codecs, wiring the remaining bit vector of each in to the next entry. */
  type DecodingContext[+A] = StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A]

  type BitVector = scodec.bits.BitVector
  val BitVector = scodec.bits.BitVector

  type ByteVector = scodec.bits.ByteVector
  val ByteVector = scodec.bits.ByteVector

  implicit val bitVectorMonoidInstance: Monoid[BitVector] = Monoid.instance(_ ++ _, BitVector.empty)

  implicit val byteVectorMonoidInstance: Monoid[ByteVector] = Monoid.instance(_ ++ _, ByteVector.empty)
}
