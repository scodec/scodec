package scodec

/**
 * Bitwise operations on a value of type `Repr`.
 *
 * @tparam Repr type that supports that supports bitwise operations
 *
 * @groupname bitwise Bitwise Operations
 * @groupprio bitwise 2
 */
trait BitwiseOperations[Repr] {

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  final def <<(n: Int): Repr = leftShift(n)

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  def leftShift(n: Int): Repr

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are sign extended.
   *
   * @group bitwise
   */
  final def >>(n: Int): Repr = rightShift(n, true)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are low.
   *
   * @group bitwise
   */
  final def >>>(n: Int): Repr = rightShift(n, false)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits.
   *
   * @param signExtension whether the `n` left-msot bits should take on the value of bit 0
   *
   * @group bitwise
   */
  def rightShift(n: Int, signExtension: Boolean): Repr

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  final def unary_~(): Repr = not

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  def not: Repr

  /**
   * Returns a bitwise AND of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def &(other: Repr): Repr = and(other)

  /**
   * Returns a bitwise AND of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def and(other: Repr): Repr

  /**
   * Returns a bitwise OR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def |(other: Repr): Repr = or(other)

  /**
   * Returns a bitwise OR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def or(other: Repr): Repr

  /**
   * Returns a bitwise XOR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def ^(other: Repr): Repr = xor(other)

  /**
   * Returns a bitwise XOR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def xor(other: Repr): Repr
}
