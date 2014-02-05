package object scodec {

  type Error = String

  // TODO implement bin/hex with macros that enforce format of parts.

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
