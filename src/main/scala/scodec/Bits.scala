package scodec

import scala.collection.immutable.BitSet
import scalaz.\/
import scalaz.\/.{left,right}

sealed trait Bits {
  import Bits._

  /**
   * Returns true if the `n`th bit is high, false otherwise.
   *
   * @throws NoSuchElementException if `n >= size`
   *
   * @group individual
   */
  def get(n: Int): Boolean

  /**
   * Returns a new bit vector with the `n`th bit high if `high` is true or low if `high` is false.
   *
   * @group individual
   */
  def updated(n: Int, high: Boolean): Bits

  /**
   * Returns number of bits in this vector.
   *
   * @group collection
   */
  def size: Long

  // derived functions

  /**
   * Returns a new bit vector representing this vector's contents followed by the specified vector's contents.
   *
   * @group collection
   */
  def ++(b: Bits): Bits = {
    if (this.size <= 256 && b.size <= 256) // coalesce small bit vectors
      this.flatten.combine(b.flatten)
    else if (this.size >= b.size) this match {
      case Append(l,r) if b.size*2 > this.size => Append(l, r ++ b)
      case _ => Append(this, b)
    }
    else b match {
      case Append(l,r) if this.size*2 > b.size => Append(this ++ l, r)
      case _ => Append(this, b)
    }
  }

  /**
   * Alias for `get`.
   *
   * @group individual
   * @see get(Int)
   */
  final def apply(n: Int): Boolean = get(n)

  /**
   * Returns a vector whose contents are the results of taking the first `n` bits of this vector.
   *
   * If this vector does not contain at least `n` bits, an error message is returned.
   *
   * @see take
   * @group collection
   */
  def acquire(n: Int): String \/ Bits =
    if (n < size) right(take(n))
    else left(s"index $n of bounds for vector of size: $size")

  /**
   * Returns a new bit vector with the `n`th bit low (and all other bits unmodified).
   *
   * @group individual
   */
  final def clear(n: Int): Bits = updated(n, false)

  /**
   * Consumes the first `n` bits of this vector and decodes them with the specified function,
   * resulting in a vector of the remaining bits and the decoded value. If this vector
   * does not have `n` bits or an error occurs while decoding, an error is returned instead.
   *
   * @group collection
   */
  def consume[A](n: Int)(decode: Bits => String \/ A): String \/ (Bits, A) =
    if (n < size) decode(take(n)).map((drop(n), _))
    else left(s"index $n of bounds for vector of size: $size")

  /**
   * Returns a vector whose contents are the results of skipping the first `n` bits of this vector and taking the rest.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def drop(n: Int): Bits =
    if (n >= size) Bits.empty
    else Drop(this, n max 0)

  /**
   * Returns a vector whose contents are the results of skipping the last `n` bits of this vector.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def dropRight(n: Int): Bits =
    if (n >= size || n < 0) this
    else Take(this, (size - n.toLong))

  /**
   * Convert this `Bits` to a flat `Bytes`.
   *
   * @group collection
   */
  def flatten: Bytes = {
    if (bytesNeededForBits(size) > Int.MaxValue)
      throw new IllegalArgumentException(s"cannot flatten bit vector of size ${size.toDouble / 8 / 1e6} MB")
    def go(b: Bits): Bytes = b match {
      case b@Bytes(_,_) => b
      case Append(l,r) => l.flatten.combine(r.flatten)
      case Take(b, n) => Bytes(go(b).bytes.take(bytesNeededForBits(n).toInt), n)
      case Drop(b, n) =>
        if (n%8 == 0) Bytes(b.flatten.bytes.drop(n / 8), b.size - n)
        else Bytes(b.flatten.bytes.drop(n/8).leftShift(n%8), b.size - n)
    }
    go(this)
  }

  /**
   * Returns true if this bit vector has no bits.
   *
   * @group collection
   */
  final def isEmpty = size == 0L

  /**
   * Returns `Some(true)` if the `n`th bit is high, `Some(false)` if low, and `None` if `n >= size`.
   *
   * @group individual
   */
  final def lift(n: Int): Option[Boolean] =
    if (n < size) Some(get(n))
    else None

  /**
   * Returns true if this bit vector has a non-zero number of bits.
   *
   * @group collection
   */
  final def nonEmpty = size > 0L

  /**
   * Returns an `n`-bit vector whose contents are this vector's contents followed by 0 or more low bits.
   *
   * @throws IllegalArgumentException if `n` < `size`
   * @group collection
   */
  def padTo(n: Long): Bits =
    if (n < size) throw new IllegalArgumentException(s"BitVector.padTo($n)")
    else this ++ Bits.fill(n - size)(false)

  /**
   * Returns a new bit vector with the `n`th bit high (and all other bits unmodified).
   *
   * @group individual
   */
  final def set(n: Int): Bits = updated(n, true)

  /**
   * Returns a vector whose contents are the results of taking the first `n` bits of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * Note: if an `n`-bit vector is required, use the `acquire` method instead.
   *
   * @see acquire
   * @group collection
   */
  def take(n: Long): Bits =
    if (n >= size) this
    else Take(this, n min size)

  /**
   * Returns a new vector of the same size with the byte order reversed.
   *
   * @group collection
   */
  def reverseByteOrder: Bits = {
    val validFinalBits = validBitsInLastByte(size)
    val invalidBits = 8 - validFinalBits
    val last = take(validFinalBits)
    val init = Bits(drop(validFinalBits.toInt).toByteVector.reverse).take(size - last.size)
    (init ++ last)
  }

  /**
   * Converts the contents of this vector to a byte vector.
   *
   * If this vector's size does not divide evenly by 8, the last byte of the returned vector
   * will be zero-padded to the right.
   *
   * @group conversions
   */
  def toByteVector: ByteVector = flatten.bytes

  /**
   * Converts the contents of this vector to a byte array.
   *
   * If this vector's size does not divide evenly by 8, the last byte of the returned vector
   * will be zero-padded to the right.
   *
   * @group conversions
   */
  def toByteArray: Array[Byte] = toByteVector.toArray

  /**
   * Converts the contents of this vector to a `java.nio.ByteBuffer`.
   *
   * The returned buffer is freshly allocated with limit set to the minimum number of bytes needed
   * to represent the contents of this vector, position set to zero, and remaining set to the limit.
   *
   * @see toByteVector
   * @group conversions
   */
  def toByteBuffer: java.nio.ByteBuffer = toByteVector.toByteBuffer

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  final def <<(n: Int): Bits = leftShift(n)

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  def leftShift(n: Int): Bits = drop(n).padTo(size)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are sign extended.
   *
   * @group bitwise
   */
  final def >>(n: Int): Bits = rightShift(n, true)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are low.
   *
   * @group bitwise
   */
  final def >>>(n: Int): Bits = rightShift(n, false)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits.
   *
   * @param signExtension whether the `n` left-most bits should take on the value of bit 0
   *
   * @group bitwise
   */
  def rightShift(n: Int, signExtension: Boolean): Bits =
    if (signExtension && lift(0).getOrElse(false))
      Bits.fill(n.toLong min size)(true) ++ dropRight(n)
    else Bits.fill(n.toLong min size)(false) ++ dropRight(n)

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  final def unary_~(): Bits = not

  private def mapBytes(f: ByteVector => ByteVector): Bits = this match {
    case Bytes(bytes, n) => Bytes(f(bytes), n)
    case Append(l,r) => Append(l.mapBytes(f), r.mapBytes(f))
    case Take(b,n) => Take(b.mapBytes(f), n)
    case Drop(b,n) => Drop(b.mapBytes(f), n)
  }

  private def zipBytesWith(other: Bits)(op: (Byte, Byte) => Int): Bits = {
    // todo: this has a much more efficient recursive algorithm -
    // only need to flatten close to leaves of the tree
    Bytes(this.flatten.bytes.zipWithI(other.flatten.bytes)(op), this.size min other.size)
  }

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  def not: Bits = mapBytes(_.not)

  /**
   * Returns a bitwise AND of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def &(other: Bits): Bits = and(other)

  /**
   * Returns a bitwise AND of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def and(other: Bits): Bits = zipBytesWith(other)(_ & _)

  /**
   * Returns a bitwise OR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def |(other: Bits): Bits = or(other)

  /**
   * Returns a bitwise OR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def or(other: Bits): Bits = zipBytesWith(other)(_ | _)

  /**
   * Returns a bitwise XOR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def ^(other: Bits): Bits = xor(other)

  /**
   * Returns a bitwise XOR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def xor(other: Bits): Bits = zipBytesWith(other)(_ ^ _)

  // impl details

  protected def checkBounds(n: Int): Unit =
    if (n >= size) outOfBounds(n)

  protected def outOfBounds(n: Int): Nothing =
    throw new NoSuchElementException(s"invalid index: $n of $size")
}

object Bits {

  val empty: Bits = Bytes(ByteVector.empty, 0)
  val zero: Bits = Bytes(ByteVector(0), 1)
  val one: Bits = Bytes(ByteVector(1), 1)
  val highByte: Bits = Bytes(ByteVector.fill(8)(1), 8)
  val lowByte: Bits = Bytes(ByteVector.fill(8)(1), 8)

  def apply(bytes: ByteVector): Bits = Bytes(bytes, bytes.size.toLong * 8)

  def fill(n: Long)(high: Boolean): Bits = {
    val needed = bytesNeededForBits(n)
    if (needed < Int.MaxValue) Bytes(ByteVector.fill(needed.toInt)(if (high) 1 else 0), n)
    else {
      fill(n / 2)(high) ++ fill(n - (n/2))(high)
    }
  }

  def getBit(byte: Byte, n: Int): Boolean =
    ((0x00000080 >> n) & byte) != 0

  def setBit(byte: Byte, n: Int, high: Boolean): Byte = {
    if (high) (0x00000080 >> n) | byte
    else (~(0x00000080 >> n)) & byte
  }.toByte

  private def validBitsInLastByte(size: Long): Int = {
    val mod = size % 8
    (if (mod == 0) 8 else mod).toInt
  }

  /** Gets a byte mask with the top `n` bits enabled. */
  private def topNBits(n: Int): Byte =
    (-1 << (8 - n)).toByte

  private def bytesNeededForBits(size: Long): Long =
    (size + 7) / 8

  private[scodec] case class Bytes(bytes: ByteVector, size: Long) extends Bits {
    private val invalidBits = 8 - validBitsInLastByte(size)
    def get(n: Int): Boolean = {
      checkBounds(n)
      getBit(bytes(n / 8), n % 8)
    }
    def updated(n: Int, high: Boolean): Bits = {
      checkBounds(n)
      val b2 = bytes.updated(
        n / 8,
        bytes.lift(n / 8).map(setBit(_, n % 8, high)).getOrElse {
          outOfBounds(n)
        }
      )
      Bytes(b2, size)
    }
    def combine(other: Bytes): Bytes = {
      val otherBytes = other.bytes
      if (isEmpty) {
        other
      } else if (otherBytes.isEmpty) {
        this
      } else if (invalidBits == 0) {
        Bytes(bytes ++ otherBytes, size + other.size)
      } else {
        val hi = bytes(bytes.size - 1)
        val otherInvalidBits = (if (other.size % 8 == 0) 0 else (8 - (other.size % 8))).toInt
        val lo = (((otherBytes.head & topNBits(8 - otherInvalidBits)) & 0x000000ff) >>> validBitsInLastByte(size)).toByte
        val updatedOurBytes = bytes.updated(bytes.size - 1, (hi | lo).toByte)
        val updatedOtherBytes = other.drop(invalidBits).toByteVector
        Bytes(updatedOurBytes ++ updatedOtherBytes, size + other.size)
      }
    }
  }

  private[scodec] case class Take(underlying: Bits, size: Long) extends Bits {
    def get(n: Int): Boolean = {
      checkBounds(n)
      underlying.get(n)
    }
    def updated(n: Int, high: Boolean): Bits = {
      checkBounds(n)
      Take(underlying.updated(n, high), size)
    }
    override def take(n: Long): Bits =
      Take(underlying, n min size)
  }
  private[scodec] case class Drop(underlying: Bits, m: Int) extends Bits {
    val size = underlying.size - m
    def get(n: Int): Boolean =
      underlying.get(m + n)
    def updated(n: Int, high: Boolean): Bits =
      Drop(underlying.updated(m + n, high), m)
    override def drop(n: Int): Bits =
      Drop(underlying, m + n)
  }
  private[scodec] case class Append(left: Bits, right: Bits) extends Bits {
    val size = left.size + right.size
    def get(n: Int): Boolean =
      if (n < left.size) left.get(n)
      else right.get(n)
    def updated(n: Int, high: Boolean): Bits =
      if (n < left.size) Append(left.updated(n, high), right)
      else Append(left, right.updated(n, high))
  }
}

