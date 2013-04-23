package scodec

import scala.collection.IndexedSeqOptimized
import scala.collection.mutable.Builder
import scalaz.\/

import java.nio.ByteBuffer


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
trait BitVector extends IndexedSeqOptimized[Boolean, BitVector] {

  /**
   * Returns true if this bit vector has no bits.
   *
   * @group collection
   */
  def isEmpty: Boolean

  /**
   * Returns true if this bit vector has a non-zero number of bits.
   *
   * @group collection
   */
  def nonEmpty: Boolean

  /**
   * Returns number of bits in this vector.
   *
   * @group collection
   */
  def size: Int

  /**
   * Alias for `get`.
   *
   * @group individual
   * @see get(Int)
   */
  final def apply(n: Int): Boolean = get(n)

  /**
   * Returns true if the `n`th bit is high, false otherwise.
   *
   * @throws NoSuchElementException if `n >= size`
   *
   * @group individual
   */
  def get(n: Int): Boolean

  /**
   * Returns `Some(true)` if the `n`th bit is high, `Some(false)` if low, and `None` if `n >= size`.
   *
   * @group individual
   */
  def lift(n: Int): Option[Boolean]

  /**
   * Returns a new bit vector with the `n`th bit high if `high` is true or low if `high` is false.
   *
   * @group individual
   */
  def updated(n: Int, high: Boolean): BitVector

  /**
   * Returns a new bit vector with the `n`th bit high (and all other bits unmodified).
   *
   * @group individual
   */
  final def set(n: Int): BitVector = updated(n, true)

  /**
   * Returns a new bit vector with the `n`th bit low (and all other bits unmodified).
   *
   * @group individual
   */
  final def clear(n: Int): BitVector = updated(n, false)

  /**
   * Returns a new bit vector representing this vector's contents followed by the specified vector's contents.
   *
   * @group collection
   */
  def ++(other: BitVector): BitVector

  /**
   * Returns a vector whose contents are the results of skipping the first `n` bits of this vector and taking the rest.
   *
   * The resulting vector's size is `0 max (size - n)`
   *
   * @group collection
   */
  def drop(n: Int): BitVector

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
  def take(n: Int): BitVector

  /**
   * Returns a vector whose contents are the results of taking the first `n` bits of this vector.
   *
   * If this vector does not contain at least `n` bits, an error message is returned.
   *
   * @see take
   * @group collection
   */
  def acquire(n: Int): String \/ BitVector

  /**
   * Consumes the first `n` bits of this vector and decodes them with the specified function,
   * resulting in a vector of the remaining bits and the decoded value. If this vector
   * does not have `n` bits or an error occurs while decoding, an error is returned instead.
   *
   * @group collection
   */
  def consume[A](n: Int)(decode: BitVector => String \/ A): String \/ (BitVector, A)

  /**
   * Returns an `n`-bit vector whose contents are this vector's contents followed by 0 or more low bits.
   *
   * @throws IllegalArgumentException if `n` < `size`
   * @group collection
   */
  def padTo(n: Int): BitVector

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  final def <<(n: Int): BitVector = leftShift(n)

  /**
   * Returns a bit vector of the same size with each bit shifted to the left `n` bits.
   *
   * @group bitwise
   */
  def leftShift(n: Int): BitVector

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are sign extended.
   *
   * @group bitwise
   */
  final def >>(n: Int): BitVector = rightShift(n, true)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits where the `n` left-most bits are low.
   *
   * @group bitwise
   */
  final def >>>(n: Int): BitVector = rightShift(n, false)

  /**
   * Returns a bit vector of the same size with each bit shifted to the right `n` bits.
   *
   * @param signExtension whether the `n` left-msot bits should take on the value of bit 0
   *
   * @group bitwise
   */
  def rightShift(n: Int, signExtension: Boolean): BitVector

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  final def unary_~(): BitVector = not

  /**
   * Returns a bitwise complement of this vector.
   *
   * @group bitwise
   */
  def not: BitVector

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
  def and(other: BitVector): BitVector

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
  def or(other: BitVector): BitVector

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
  def xor(other: BitVector): BitVector

  /**
   * Converts the contents of this vector to a byte vector.
   *
   * If this vector's size does not divide evenly by 8, the last byte of the returned vector
   * will be zero-padded to the right.
   *
   * @group conversions
   */
  def asBytes: ByteVector

  /**
   * Converts the contents of this vector to a `java.nio.ByteBuffer`.
   *
   * The returned buffer is freshly allocated with limit set to the minimum number of bytes needed
   * to represent the contents of this vector, position set to zero, and remaining set to the limit.
   *
   * @see asBytes
   * @group conversions
   */
  def asByteBuffer: ByteBuffer
}

object BitVector {

  val empty: BitVector = BitVector(ByteVector.empty)

  def high(n: Int): BitVector = apply(n, ByteVector.fill((n + 7) / 8)(-1))
  def low(n: Int): BitVector = apply(n, ByteVector.fill((n + 7) / 8)(0))

  private def apply(n: Int, bytes: ByteVector): BitVector =
    new SimpleBitVector(n, clearUnneededBits(n, bytes))

  def apply(bytes: ByteVector): BitVector = new SimpleBitVector(bytes.size * 8, bytes)
  def apply(bytes: Array[Byte]): BitVector = apply(ByteVector(bytes))
  def apply(buffer: ByteBuffer): BitVector = apply(ByteVector(buffer))
  def apply[A: Integral](bytes: A*): BitVector = apply(ByteVector(bytes: _*))

  private def getBit(byte: Byte, n: Int): Boolean =
    ((0x00000080 >> n) & byte) != 0

  private def setBit(byte: Byte, n: Int, high: Boolean): Byte = {
    if (high) (0x00000080 >> n) | byte
    else (~(0x00000080 >> n)) & byte
  }.toByte

  /** Gets a byte mask with the top `n` bits enabled. */
  private def topNBits(n: Int): Byte =
    (-1 << (8 - n)).toByte

  private def bytesNeededForBits(size: Int): Int =
    (size + 7) / 8

  /** Number of bits (1 - 8) in the last byte of a vector of the specified size. */
  private def validBitsInLastByte(size: Int): Int = {
    val mod = size % 8
    if (mod == 0) 8 else mod
  }

  /** Clears (sets to 0) any bits in the last byte that are not used for storing `size` bits. */
  private def clearUnneededBits(size: Int, bytes: ByteVector): ByteVector = {
    val valid = validBitsInLastByte(size)
    if (valid < 8) {
      val idx = bytes.size - 1
      val last = bytes(idx)
      bytes.updated(idx, (last & topNBits(valid)).toByte)
    } else {
      bytes
    }
  }


  private class SimpleBitVector(val length: Int, bytes: ByteVector) extends BitVector with Serializable {

    require(size >= 0, "size must be non-negative")
    require(bytes.size == bytesNeededForBits(size), s"size ($size) and bytes.size (${bytes.size}) are not compatible")

    /** Number of bits (0 - 7) in the last bye of bytes that are not part of vector. */
    private val invalidBits = 8 - validBitsInLastByte(size)

    def get(n: Int) = lift(n) getOrElse { throw new NoSuchElementException(s"Cannot get bit $n from vector of $size bits") }

    def lift(n: Int) = for {
      byte <- bytes.lift(n / 8)
    } yield getBit(byte, n % 8)

    def updated(n: Int, high: Boolean): BitVector =
      new SimpleBitVector(size, bytes.updated(n / 8, setBit(bytes(n / 8), n % 8, high)))

    override def slice(from: Int, until: Int): BitVector = {
      val low = from max 0
      val high = until max 0 min size
      val newSize = (high - low) max 0

      if (newSize == 0) {
        BitVector.empty
      } else {
        val bitsToShiftEachByte = low % 8
        val lowByte = low / 8
        val shiftedByWholeBytes = bytes.slice(lowByte, lowByte + bytesNeededForBits(newSize) + 1)
        val newBytes = (shiftedByWholeBytes zipWithI (shiftedByWholeBytes.drop(1) :+ (0: Byte))) { case (a, b) =>
          val hi = (a << bitsToShiftEachByte)
          val low = (((b & topNBits(bitsToShiftEachByte)) & 0x000000ff) >>> (8 - bitsToShiftEachByte))
          hi | low
        }
        BitVector(newSize, if (newSize <= (newBytes.size - 1) * 8) newBytes.dropRight(1) else newBytes)
      }
    }

    def acquire(n: Int): String \/ BitVector = {
      if (size < n) \/ left s"cannot acquire $n bits from a vector that contains $size bits"
      else \/ right take(n)
    }

    def consume[A](n: Int)(decode: BitVector => String \/ A): String \/ (BitVector, A) = for {
      toDecode <- acquire(n)
      decoded <- decode(toDecode)
    } yield (drop(n), decoded)

    def padTo(n: Int) = {
      if (n <= size) this
      else this ++ BitVector.low(n - size)
    }

    def ++(other: BitVector): BitVector = {
      val otherBytes = other.asBytes
      if (isEmpty) {
        other
      } else if (otherBytes.isEmpty) {
        this
      } else if (invalidBits == 0) {
        new SimpleBitVector(size + other.size, bytes ++ otherBytes)
      } else {
        val hi = bytes(bytes.size - 1)
        val otherInvalidBits = if (other.size % 8 == 0) 0 else (8 - (other.size % 8))
        val lo = (((otherBytes.head & topNBits(8 - otherInvalidBits)) & 0x000000ff) >>> otherInvalidBits).toByte
        val updatedOurBytes = bytes.updated(bytes.size - 1, (hi | lo).toByte)
        val updatedOtherBytes = other.drop(invalidBits).asBytes
        new SimpleBitVector(size + other.size, updatedOurBytes ++ updatedOtherBytes)
      }
    }

    def leftShift(n: Int): BitVector = {
      if (n <= 0) this
      else if (n >= size) BitVector.low(size)
      else drop(n) ++ BitVector.low(n)
    }

    def rightShift(n: Int, signExtension: Boolean): BitVector = {
      if (isEmpty || n <= 0) this
      else {
        val extensionHigh = signExtension && (bytes.head & 0x00000080) != 0
        if (n >= size) {
          if (extensionHigh) BitVector.high(size) else BitVector.low(size)
        } else {
          (if (extensionHigh) BitVector.high(n) else BitVector.low(n)) ++ take(size - n)
        }
      }
    }

    def not: BitVector =
      BitVector(size, bytes mapI { ~_ })

    def and(other: BitVector): BitVector =
      zipBytesWith(other)(_ & _)

    def or(other: BitVector): BitVector =
      zipBytesWith(other)(_ | _)

    def xor(other: BitVector): BitVector =
      zipBytesWith(other)(_ ^ _)

    private def zipBytesWith(other: BitVector)(op: (Byte, Byte) => Int): BitVector =
      BitVector(size min other.size, (bytes zipWithI other.asBytes)(op))

    def asBytes = bytes

    def asByteBuffer = ByteBuffer.wrap(bytes.toArray)

    def seq: IndexedSeq[Boolean] = {
      val bldr = Vector.newBuilder[Boolean]
      for (i <- 0 until length)
        bldr += get(i)
      bldr.result
    }

    override protected[this] def thisCollection = seq

    override def hashCode: Int =
      bytes.hashCode

    override def equals(other: Any): Boolean = other match {
      case o: BitVector => bytes == o.asBytes
      case _ => false
    }

    override def toString = {
      if (isEmpty) {
        "BitVector(0 bits)"
      } else {
        val hex = Bytes.toHexadecimal(bytes.toIterable)
        val truncatedHex = if (invalidBits >= 4) {
          hex.substring(0, hex.size - 1)
        } else hex
        s"BitVector($size bits, $truncatedHex)"
      }
    }

    override protected[this] def newBuilder: Builder[Boolean, BitVector] = new Builder[Boolean, BitVector] {
      private var length = 0
      private var currentByte: Byte = 0
      private var doneBytes = Vector.newBuilder[Byte]

      def +=(bit: Boolean) = {
        currentByte = setBit(currentByte, length, bit)
        length += 1
        this
      }

      def clear() {
        length = 0
        currentByte = 0
        doneBytes.clear()
      }

      def result(): BitVector = {
        if (length == 0) {
          BitVector.empty
        } else {
          val bytes = (doneBytes += currentByte).result()
          new SimpleBitVector(length, ByteVector(bytes))
        }
      }
    }
  }
}
