package scodec
package codecs

import scodec.bits.BitVector

/** Indicates a checksum over `bits` did not match the expected value. */
case class ChecksumMismatch(bits: BitVector, expected: BitVector, actual: BitVector, context: List[String] = Nil) extends Err {
  def message: String = s"checksum mismatch for bits: $bits, expected: $expected, actual: $actual"
  def pushContext(ctx: String): Err = copy(context = ctx :: context)
}
