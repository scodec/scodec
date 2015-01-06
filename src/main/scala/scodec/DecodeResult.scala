package scodec

import scodec.bits.BitVector

/**
 * Result of a decoding operation, which consists of the decoded value and the remaining bits that were not consumed by decoding.
 *
 * @groupname Ungrouped Members
 * @groupprio 1
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 0
 */
case class DecodeResult[+A](value: A, remainder: BitVector) {

  /** Maps the supplied function over the decoded value. */
  def mapValue[B](f: A => B): DecodeResult[B] = DecodeResult(f(value), remainder)

  /** Maps the supplied function over the remainder. */
  def mapRemainder(f: BitVector => BitVector): DecodeResult[A] = DecodeResult(value, f(remainder))
}
