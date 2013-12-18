package scodec

import java.nio.ByteBuffer
import scala.collection.immutable.BitSet
import scalaz.\/
import scalaz.\/.{left,right}

/**
 * Persistent vector of bits, stored as bytes.
 *
 * Bits are numbered left to right, starting at 0.
 *
 * @groupname collection Collection Like Methods
 * @groupprio collection 0
 *
 * @groupname individual Operations on Individual Bits
 * @groupprio individual 1
 *
 * @groupname bitwise Bitwise Operations
 * @groupprio bitwise 2
 *
 * @groupname conversions Conversions
 * @groupprio conversions 3
 */
sealed trait BitVector {
  import BitVector._

  /**
   * Returns true if the `n`th bit is high, false otherwise.
   *
   * @throws NoSuchElementException if `n >= size`
   *
   * @group individual
   */
  def get(n: Long): Boolean

  /**
   * Returns a new bit vector with the `n`th bit high if `high` is true or low if `high` is false.
   *
   * @group individual
   */
  def updated(n: Long, high: Boolean): BitVector

  /**
   * Returns number of bits in this vector.
   *
   * @group collection
   */
  def size: Long
  def intSize: Option[Int] = if (size <= Int.MaxValue) Some(size.toInt) else None

  // derived functions

  /**
   * Returns a new bit vector representing this vector's contents followed by the specified vector's contents.
   *
   * @group collection
   */
  def ++(b: BitVector): BitVector = {
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
   * @see get(Long)
   */
  final def apply(n: Long): Boolean = get(n)

  /**
   * Returns a vector whose contents are the results of taking the first `n` bits of this vector.
   *
   * If this vector does not contain at least `n` bits, an error message is returned.
   *
   * @see take
   * @group collection
   */
  def acquire(n: Long): String \/ BitVector =
    if (n < size) right(take(n))
    else left(s"index $n of bounds for vector of size: $size")

  /**
   * Returns a new bit vector with the `n`th bit low (and all other bits unmodified).
   *
   * @group individual
   */
  final def clear(n: Long): BitVector = updated(n, false)

  /**
   * Consumes the first `n` bits of this vector and decodes them with the specified function,
   * resulting in a vector of the remaining bits and the decoded value. If this vector
   * does not have `n` bits or an error occurs while decoding, an error is returned instead.
   *
   * @group collection
   */
  def consume[A](n: Long)(decode: BitVector => String \/ A): String \/ (BitVector, A) =
    if (n < size) decode(take(n)).map((drop(n), _))
    else left(s"index $n of bounds for vector of size: $size")

  /**
   * Returns a vector whose contents are the results of skipping the first `n` bits of this vector and taking the rest.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def drop(n: Long): BitVector =
    if (n >= size) BitVector.empty
    else Drop(this, n max 0)

  /**
   * Returns a vector whose contents are the results of skipping the last `n` bits of this vector.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def dropRight(n: Long): BitVector =
    if (n >= size || n < 0) this
    else Take(this, (size - n.toLong))

  /**
   * Convert this `BitVector` to a flat `Bytes`.
   *
   * @group collection
   */
  def flatten: Bytes = {
    if (bytesNeededForBits(size) > Int.MaxValue)
      throw new IllegalArgumentException(s"cannot flatten bit vector of size ${size.toDouble / 8 / 1e9} GB")
    def go(b: BitVector): Bytes = b match {
      case b@Bytes(_,_) => b
      case Append(l,r) => l.flatten.combine(r.flatten)
      case Take(b, n) => Bytes(go(b).bytes.take(bytesNeededForBits(n).toInt), n)
      case Drop(b, n) =>
        if (n == 0) go(b)
        else if (n%8 == 0) Bytes(b.flatten.bytes.drop((n / 8).toInt), b.size - n)
        else {
          val b2 = b.flatten.bytes.drop((n/8).toInt)
          val (hd, tl) = (b2.take(1), b2.drop(1))
          val out = Bytes(ByteVector(hd.head << (n%8)), 8-(n%8)).combine(Bytes(tl, tl.size * 8))
          Bytes(clearUnneededBits(b.size - n, out.bytes), b.size - n)
        }
    }
    go(this)
  }

  /**
   * Returns the first bit in this vector.
   *
   * @throws IllegalArgumentException if this vector is empty
   * @group individual
   */
  def head: Boolean = get(0)

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
  final def lift(n: Long): Option[Boolean] =
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
  def padTo(n: Long): BitVector =
    if (n < size) throw new IllegalArgumentException(s"BitVector.padTo($n)")
    else this ++ BitVector.fill(n - size)(false)

  /**
   * Reverse the bits of this vector.
   */
  def reverse: BitVector =
    // todo: this has a log time implementation, assuming a balanced tree
    BitVector(flatten.bytes.reverse.map(reverseBitsInBytes _)).drop(8 - validBitsInLastByte(size))

  /**
   * Returns a new bit vector with the `n`th bit high (and all other bits unmodified).
   *
   * @group individual
   */
  final def set(n: Long): BitVector = updated(n, true)

  /**
   * Return the sequence of bits in this vector.
   *
   * @throws IllegalArgumentException if this vector's size exceeds Int.MaxValue
   * @see acquire
   * @group collection
   */
  def toIndexedSeq: IndexedSeq[Boolean] = {
    if (size > Int.MaxValue) throw new IllegalArgumentException(s"vector too big for IndexedSeq: $size")
    val bldr = Vector.newBuilder[Boolean]
    for (i <- 0 until size.toInt)
      bldr += get(i)
    bldr.result
  }

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
  def take(n: Long): BitVector =
    if (n >= size) this
    else Take(this, n min size)

  /**
   * Returns a vector whose contents are the results of taking the last `n` bits of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * Note: if an `n`-bit vector is required, use the `acquire` method instead.
   *
   * @see acquire
   * @group collection
   */
  def takeRight(n: Long): BitVector =
    if (n < 0) throw new IllegalArgumentException(s"takeRight($n)")
    else if (n >= size) this
    else Drop(this, size-n)

  /**
   * Return the sequence of bits in this vector.
   *
   * @throws IllegalArgumentException if this vector's size exceeds Int.MaxValue
   * @see acquire
   * @group collection
   */
  def toIterable: Iterable[Boolean] =
    intSize.map(n => (0 until n).map(get(_)))
           .getOrElse(throw new IllegalArgumentException(s"BitVector too big for Iterable: $size"))

  /**
   * Returns a new vector of the same size with the byte order reversed.
   *
   * @group collection
   */
  def reverseByteOrder: BitVector = {
    val validFinalBits = validBitsInLastByte(size)
    val invalidBits = 8 - validFinalBits
    val last = take(validFinalBits)
    val init = BitVector(drop(validFinalBits.toInt).toByteVector.reverse).take(size - last.size)
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
  final def <<(n: Long): BitVector = leftShift(n)

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  def leftShift(n: Long): BitVector = drop(n).padTo(size)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are sign extended.
   *
   * @group bitwise
   */
  final def >>(n: Long): BitVector = rightShift(n, true)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are low.
   *
   * @group bitwise
   */
  final def >>>(n: Long): BitVector = rightShift(n, false)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits.
   *
   * @param signExtension whether the `n` left-most bits should take on the value of bit 0
   *
   * @group bitwise
   */
  def rightShift(n: Long, signExtension: Boolean): BitVector =
    if (signExtension && lift(0).getOrElse(false))
      BitVector.fill(n.toLong min size)(true) ++ dropRight(n)
    else BitVector.fill(n.toLong min size)(false) ++ dropRight(n)

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  final def unary_~(): BitVector = not

  private def mapBytes(f: ByteVector => ByteVector): BitVector = this match {
    case Bytes(bytes, n) => Bytes(f(bytes), n)
    case Append(l,r) => Append(l.mapBytes(f), r.mapBytes(f))
    case Take(b,n) => Take(b.mapBytes(f), n)
    case Drop(b,n) => Drop(b.mapBytes(f), n)
  }

  private def zipBytesWith(other: BitVector)(op: (Byte, Byte) => Int): BitVector = {
    // todo: this has a much more efficient recursive algorithm -
    // only need to flatten close to leaves of the tree
    Bytes(this.flatten.bytes.zipWithI(other.flatten.bytes)(op), this.size min other.size)
  }

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  def not: BitVector = mapBytes(_.not)

  /**
   * Returns a bitwise AND of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def &(other: BitVector): BitVector = and(other)

  /**
   * Returns a bitwise AND of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def and(other: BitVector): BitVector = zipBytesWith(other)(_ & _)

  /**
   * Returns a bitwise OR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def |(other: BitVector): BitVector = or(other)

  /**
   * Returns a bitwise OR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def or(other: BitVector): BitVector = zipBytesWith(other)(_ | _)

  /**
   * Returns a bitwise XOR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  final def ^(other: BitVector): BitVector = xor(other)

  /**
   * Returns a bitwise XOR of this vector with the specified vector.
   *
   * The resulting vector's size is the minimum of this vector's size and the specified vector's size.
   *
   * @group bitwise
   */
  def xor(other: BitVector): BitVector = zipBytesWith(other)(_ ^ _)

  override def equals(other: Any): Boolean = other match {
    case o: BitVector => toIndexedSeq == o.toIndexedSeq // toByteVector == o.toByteVector
    case _ => false
  }
  override def toString =
    "BitVector("+toIndexedSeq.map(b => if (b) 1 else 0).mkString+")"

  // impl details

  protected def checkBounds(n: Long): Unit =
    if (n >= size) outOfBounds(n)

  protected def outOfBounds(n: Long): Nothing =
    throw new NoSuchElementException(s"invalid index: $n of $size")
}

object BitVector {

  val empty: BitVector = Bytes(ByteVector.empty, 0)
  val zero: BitVector = Bytes(ByteVector(0), 1)
  val one: BitVector = Bytes(ByteVector(1), 1)
  val highByte: BitVector = Bytes(ByteVector.fill(8)(1), 8)
  val lowByte: BitVector = Bytes(ByteVector.fill(8)(0), 8)

  def high(n: Long): BitVector = fill(n)(true)
  def low(n: Long): BitVector = fill(n)(false)

  def apply(bytes: ByteVector): BitVector = Bytes(bytes, bytes.size.toLong * 8)
  def apply(buffer: ByteBuffer): BitVector = apply(ByteVector(buffer))
  def apply(bytes: Array[Byte]): BitVector = Bytes(ByteVector(bytes), bytes.size.toLong * 8)
  def apply[A: Integral](bytes: A*): BitVector = apply(ByteVector(bytes: _*))

  def fill(n: Long)(high: Boolean): BitVector = {
    val needed = bytesNeededForBits(n)
    if (needed < Int.MaxValue) {
      val bytes = ByteVector.fill(needed.toInt)(if (high) -1 else 0)
      Bytes(clearUnneededBits(n, bytes), n)
    }
    else {
      fill(n / 2)(high) ++ fill(n - (n/2))(high)
    }
  }

  private[scodec] case class Bytes(bytes: ByteVector, size: Long) extends BitVector {
    private val invalidBits = 8 - validBitsInLastByte(size)
    def get(n: Long): Boolean = {
      checkBounds(n)
      getBit(bytes((n / 8).toInt), (n % 8).toInt)
    }
    def updated(n: Long, high: Boolean): BitVector = {
      checkBounds(n)
      val b2 = bytes.updated(
        (n / 8).toInt,
        bytes.lift((n / 8).toInt).map(setBit(_, (n % 8).toInt, high)).getOrElse {
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

  private[scodec] case class Take(underlying: BitVector, size: Long) extends BitVector {
    def get(n: Long): Boolean = {
      checkBounds(n)
      underlying.get(n)
    }
    def updated(n: Long, high: Boolean): BitVector = {
      checkBounds(n)
      Take(underlying.updated(n, high), size)
    }
    override def take(n: Long): BitVector =
      Take(underlying, n min size)
  }
  private[scodec] case class Drop(underlying: BitVector, m: Long) extends BitVector {
    val size = underlying.size - m
    def get(n: Long): Boolean =
      underlying.get(m + n)
    def updated(n: Long, high: Boolean): BitVector =
      Drop(underlying.updated(m + n, high), m)
    override def drop(n: Long): BitVector =
      Drop(underlying, m + n)
  }
  private[scodec] case class Append(left: BitVector, right: BitVector) extends BitVector {
    val size = left.size + right.size
    def get(n: Long): Boolean =
      if (n < left.size) left.get(n)
      else right.get(n)
    def updated(n: Long, high: Boolean): BitVector =
      if (n < left.size) Append(left.updated(n, high), right)
      else Append(left, right.updated(n, high))
  }

  implicit val monoidInstance: scalaz.Monoid[BitVector] = new scalaz.Monoid[BitVector] {
    override def zero: BitVector = BitVector.empty
    override def append(x: BitVector, y: => BitVector) = x ++ y
  }

  // bit twiddling operations

  private def getBit(byte: Byte, n: Int): Boolean =
    ((0x00000080 >> n) & byte) != 0

  private def setBit(byte: Byte, n: Int, high: Boolean): Byte = {
    if (high) (0x00000080 >> n) | byte
    else (~(0x00000080 >> n)) & byte
  }.toByte

  private def validBitsInLastByte(size: Long): Long = {
    val mod = size % 8
    (if (mod == 0) 8 else mod).toLong
  }

  /** Gets a byte mask with the top `n` bits enabled. */
  private def topNBits(n: Int): Byte =
    (-1 << (8 - n)).toByte

  private def bytesNeededForBits(size: Long): Long =
    (size + 7) / 8

  private def reverseBitsInBytes(b: Byte): Byte = {
    // See Hacker's Delight Chapter 7 page 101
    var x = (b & 0x055) << 1 | (b & 0x0aa) >> 1
    x = (x & 0x033) << 2 | (x & 0x0cc) >> 2
    x = (x & 0x00f) << 4 | (x & 0x0f0) >> 4
    x.toByte
  }

  /** Clears (sets to 0) any bits in the last byte that are not used for storing `size` bits. */
  private def clearUnneededBits(size: Long, bytes: ByteVector): ByteVector = {
    val valid = validBitsInLastByte(size).toInt
    if (valid < 8) {
      val idx = bytes.size - 1
      val last = bytes(idx)
      bytes.updated(idx, (last & topNBits(valid)).toByte)
    } else {
      bytes
    }
  }
}

