import scalaz.{ \/, StateT }

package object scodec {

  type Error = String

  /** Alias for state/either transformer that simplifies calling decode on a series of codecs, wiring the remaining bit vector of each in to the next entry. */
  type DecodingContext[+A] = StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A]

  /** Implicit conversion from `ByteVector` to `BitVector`. */
  implicit def byteVectorToBitVector(byteVector: ByteVector): BitVector = byteVector.toBitVector

  /**
   * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
   *
   * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
   * of type `BitVector`.
   */
  implicit class BinStringSyntax(val sc: StringContext) extends AnyVal {
    def bin(args: BitVector*): BitVector = macro LiteralSyntaxMacros.binStringInterpolator
  }

  /**
   * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
   *
   * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
   * of type `ByteVector`.
   */
  implicit class HexStringSyntax(val sc: StringContext) extends AnyVal {
    def hex(args: ByteVector*): ByteVector = macro LiteralSyntaxMacros.hexStringInterpolator
  }
}
