package object scodec {

  type Error = String

  /**
   * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
   *
   * Named arguments are supported in the same manner as the standard `s` interpolator.
   */
  implicit class BinStringSyntax(val sc: StringContext) extends AnyVal {
    def bin(args: Any*): BitVector = BitVector.fromValidBin(sc.s(args: _*))
  }

  /**
   * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
   *
   * Named arguments are supported in the same manner as the standard `s` interpolator.
   */
  implicit class HexStringSyntax(val sc: StringContext) extends AnyVal {
    def hex(args: Any*): ByteVector = ByteVector.fromValidHex(sc.s(args: _*))
  }
}
