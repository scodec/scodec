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
  def ++(b2: BitVector): BitVector = {
    def go(x: BitVector, y: BitVector, force: Boolean = false): BitVector =
      //if ((((x.size+y.size) % 8 == 0) && x.size <= 256 && y.size <= 256) ||
      //    ((x.size >= 256 && x.size < 512 && y.size >= 256 && y.size > 512)))
      if ((((x.size+y.size) % 8 == 0) && x.size <= 16 && y.size <= 16) ||
          ((x.size >= 16 && x.size < 32 && y.size >= 16 && y.size > 32)))
      // if (false)
        // coalesce small bit vectors, preferring to obtain a byte-aligned result
        x.flatten.combine(y.flatten)
      else if (x.size >= y.size) x match {
        case Append(l,r) if (x.size - y.size) >
                            (r.size - y.size).abs =>
          val r2 = r ++ y
          // if the branches are not of roughly equal size,
          // reinsert the left branch from the top
          if (force || l.size*2 > r2.size) Append(l, r2)
          else go(l, r2, force = true)
        case _ => Append(x, y)
      }
      else y match {
        case Append(l,r) if (y.size - x.size) >
                            (r.size - x.size).abs =>
          val l2 = x ++ l
          if (force || r.size*2 > l2.size) Append(l2, r)
          else go(l2, r, force = true)
        case _ => Append(x, y)
      }
    if (b2.isEmpty) this
    else if (this.isEmpty) b2
    else go(this, b2)
  }

  def depthExceeds(d: Int): Boolean = {
    def go(node: BitVector, cur: Int): Boolean =
      (cur > d) ||
      (node match {
        case Append(l,r) => go(l, cur+1) || go(r, cur+1)
        case Drop(u,n) => go(u, cur+1)
        case Bytes(b,n) => false
      })
    go(this, 0)
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
    if (n <= size) right(take(n))
    else left(s"cannot acquire $n bits from a vector that contains $size bits")

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
     for {
       toDecode <- acquire(n)
       decoded <- decode(toDecode)
     } yield (drop(n), decoded)

  /**
   * Returns a vector whose contents are the results of skipping the first `n` bits of this vector and taking the rest.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def drop(n: Long): BitVector =
    if (n >= size) BitVector.empty
    else if (n <= 0) this
    else this match {
      case Bytes(bytes, m) =>
        if (n % 8 == 0) Bytes(bytes.drop((n/8).toInt), m-n)
        else Drop(this.flatten, n)
      case Append(l,r) =>
        if (l.size <= n) r.drop(n - l.size)
        else l.drop(n) ++ r
      case Drop(bytes, m) =>
        bytes.drop(m + n)
    }

  /**
   * Returns a vector whose contents are the results of skipping the last `n` bits of this vector.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def dropRight(n: Long): BitVector =
    if (n <= 0) this
    else if (n >= size) BitVector.empty
    else take(size - n)

  /**
   * Convert this `BitVector` to a flat `Bytes`.
   *
   * @group collection
   */
  def flatten: Bytes = {
    if (bytesNeededForBits(size) > Int.MaxValue)
      throw new IllegalArgumentException(s"cannot flatten bit vector of size ${size.toDouble / 8 / 1e9} GB")
    def go(b: BitVector): Bytes = b match {
      case Bytes(x,n) => Bytes(x,n)
      case Append(l,r) => l.flatten.combine(r.flatten)
      case Drop(b, from) =>
        val low = from max 0
        val newSize = b.size - low
        if (newSize == 0) BitVector.empty.flatten
        else {
          val lowByte = (low / 8).toInt
          val shiftedByWholeBytes = b.flatten.bytes.slice(lowByte, lowByte + bytesNeededForBits(newSize).toInt + 1)
          val bitsToShiftEachByte = (low % 8).toInt
          val newBytes = {
            if (bitsToShiftEachByte == 0) shiftedByWholeBytes
            else {
              (shiftedByWholeBytes zipWithI (shiftedByWholeBytes.drop(1) :+ (0: Byte))) { case (a, b) =>
                val hi = (a << bitsToShiftEachByte)
                val low = (((b & topNBits(bitsToShiftEachByte)) & 0x000000ff) >>> (8 - bitsToShiftEachByte))
                hi | low
              }
            }
          }
          Bytes(if (newSize <= (newBytes.size - 1) * 8) newBytes.dropRight(1) else newBytes, newSize)
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
   * @throws IllegalArgumentException if `n < size`
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
   * Returns a vector whose contents are the results of taking the first `n` bits of this vector.
   *
   * The resulting vector's size is `n min size`.
   *
   * Note: if an `n`-bit vector is required, use the `acquire` method instead.
   *
   * @see acquire
   * @group collection
   */
  def take(n0: Long): BitVector = {
    val n = n0 max 0
    if (n >= size) this
    else if (n == 0) BitVector.empty
    else this match {
      case Bytes(underlying, m) =>
        // eagerly trim from underlying here
        val m2 = n min m
        val underlyingN = bytesNeededForBits(m2).toInt
        Bytes(underlying.take(underlyingN), m2)
      case Drop(underlying, m) => underlying.take(m + n).drop(m)
      case Append(l, r) =>
        if (n <= l.size) l.take(n)
        else l ++ r.take(n-l.size)
    }
  }

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
    else this.drop(size-n)

  /**
   * Return the sequence of bits in this vector. The returned
   * `IndexedSeq` is just a view; nothing is actually copied.
   *
   * @throws IllegalArgumentException if this vector's size exceeds Int.MaxValue
   * @see acquire
   * @see toIndexedSeq
   * @group collection
   */
  def toIndexedSeq: IndexedSeq[Boolean] = {
    intSize.map { n =>
      new IndexedSeq[Boolean] {
        def length = BitVector.this.size.toInt
        def apply(idx: Int): Boolean = BitVector.this.get(idx.toLong)
      }
    }.getOrElse {
      throw new IllegalArgumentException(s"BitVector too big for Seq: $size")
    }
  }

  /**
   * Returns a new vector of the same size with the byte order reversed.
   *
   * @group collection
   */
  def reverseByteOrder: BitVector = {
    if (size % 8 == 0) Bytes(flatten.bytes.reverse, size)
    else {
      val validFinalBits = validBitsInLastByte(size)
      val last = take(validFinalBits).flatten
      val b = drop(validFinalBits).toByteVector.reverse
      val init = Bytes(b, size-last.size)
      val res = (init ++ last)
      require(res.size == size)
      res
    }
  }

  /**
   * Converts the contents of this vector to a byte vector.
   *
   * If this vector's size does not divide evenly by 8, the last byte of the returned vector
   * will be zero-padded to the right.
   *
   * @group conversions
   */
  def toByteVector: ByteVector =
    flatten.bytes

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
  def leftShift(n: Long): BitVector =
    if (n <= 0) this
    else if (n >= size) BitVector.low(size)
    else drop(n) ++ BitVector.low(n)

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
  def rightShift(n: Long, signExtension: Boolean): BitVector = {
    if (isEmpty || n <= 0) this
    else {
      val extensionHigh = signExtension && head
      if (n >= size) {
        if (extensionHigh) BitVector.high(size) else BitVector.low(size)
      }
      else {
        (if (extensionHigh) BitVector.high(n) else BitVector.low(n)) ++
        take(size - n)
      }
    }
  }

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  final def unary_~(): BitVector = not

  private def mapBytes(f: ByteVector => ByteVector): BitVector = this match {
    case Bytes(bytes, n) => Bytes(f(bytes), n)
    case Append(l,r) => Append(l.mapBytes(f), r.mapBytes(f))
    case Drop(b,n) => Drop(b.mapBytes(f).flatten, n)
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
    case o: BitVector if size == o.size => {
      var i = 0L
      while (i < size) {
        if (get(i) != o.get(i)) return false
        i += 1
      }
      return true
    }
    case _ => false
  }

  def pretty(prefix: String): String = this match {
    case Append(l,r) => prefix + "append\n" +
                        l.pretty(prefix + "  ") + "\n" +
                        r.pretty(prefix + "  ")
    case Bytes(b, n) =>
      if (n > 16) prefix + s"bits $n #:${b.hashCode}"
      else        prefix + s"bits $n 0x${b.toHex}"
    case Drop(u, n) => prefix + s"drop ${n}\n" +
                       u.pretty(prefix + "  ")
  }

  override def toString = "\n" + pretty("")
    // "BitVector("+toIndexedSeq.map(b => if (b) 1 else 0).mkString+")"

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

  def bit(high: Boolean): BitVector = fill(1)(high)

  def bits(b: Iterable[Boolean]): BitVector =
    b.zipWithIndex.foldLeft(low(b.size))((acc,b) =>
      acc.updated(b._2, b._1)
    )

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
      Bytes(bytes, n)
    }
    else {
      fill(n / 2)(high) ++ fill(n - (n/2))(high)
    }
  }

  object Bytes {
    def apply(bytes: ByteVector, size: Long): Bytes = {
      val needed = bytesNeededForBits(size)
      require(needed <= bytes.size)
      val b = if (bytes.size > needed) bytes.take(needed.toInt) else bytes
      new Bytes(clearUnneededBits(size, b), size)
    }

    def unapply(b: BitVector): Option[(ByteVector, Long)] = b match {
      case bs: Bytes => Some((bs.bytes, bs.size))
      case _ => None
    }
  }
  private[scodec] class Bytes(val bytes: ByteVector, val size: Long) extends BitVector {
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
        val bytesCleared = bytes
        val hi = bytesCleared(bytesCleared.size - 1)
        val otherInvalidBits = (if (other.size % 8 == 0) 0 else (8 - (other.size % 8))).toInt
        val lo = (((otherBytes.head & topNBits(invalidBits.toInt)) & 0x000000ff) >>> validBitsInLastByte(size)).toByte
        //val lo = (((otherBytes.head & topNBits(8 - otherInvalidBits)) & 0x000000ff) >>> validBitsInLastByte(size)).toByte
        val updatedOurBytes = bytesCleared.updated(bytesCleared.size - 1, (hi | lo).toByte)
        val updatedOtherBytes = other.drop(invalidBits).toByteVector
        Bytes(updatedOurBytes ++ updatedOtherBytes, size + other.size)
      }
    }
  }

  private[scodec] case class Drop(underlying: Bytes, m: Long) extends BitVector {
    val size = underlying.size - m
    def get(n: Long): Boolean =
      underlying.get(m + n)
    def updated(n: Long, high: Boolean): BitVector =
      Drop(underlying.updated(m + n, high).flatten, m)
  }
  private[scodec] case class Append(left: BitVector, right: BitVector) extends BitVector {
    val size = left.size + right.size
    def get(n: Long): Boolean =
      if (n < left.size) left.get(n)
      else right.get(n - left.size)
    def updated(n: Long, high: Boolean): BitVector =
      if (n < left.size) Append(left.updated(n, high), right)
      else Append(left, right.updated(n - left.size, high))
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
    (if (mod == 0) 8 else mod)
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
    if (bytes.nonEmpty && valid < 8) {
      val idx = bytes.size - 1
      val last = bytes(idx)
      bytes.updated(idx, (last & topNBits(valid)).toByte)
    } else {
      bytes
    }
  }
}

