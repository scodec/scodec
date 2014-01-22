package scodec

import scala.collection.{IndexedSeqLike, IndexedSeqOptimized}
import scalaz.{\/, Monoid}
import scalaz.std.vector._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import java.nio.ByteBuffer


trait ByteVector extends IndexedSeqOptimized[Byte, ByteVector] with BitwiseOperations[ByteVector] {

  def lift(idx: Int): Option[Byte]

  def updated(idx: Int, b: Byte): ByteVector

  def +:(byte: Byte): ByteVector

  def :+(byte: Byte): ByteVector

  def ++(other: ByteVector): ByteVector

  def map(f: Byte => Byte): ByteVector

  def mapI(f: Byte => Int): ByteVector =
    map(f andThen { _.toByte })

  def zipWith(other: ByteVector)(op: (Byte, Byte) => Byte): ByteVector

  def zipWithI(other: ByteVector)(op: (Byte, Byte) => Int): ByteVector =
    zipWith(other) { case (l, r) => op(l, r).toByte }

  def toArray: Array[Byte]

  def toByteBuffer: ByteBuffer = ByteBuffer.wrap(toArray)

  def toBitVector: BitVector = BitVector(this)

  /** Converts the contents of this byte vector to a hexadecimal string of `size * 2` nibbles.  */
  def toHex: String = {
    import ByteVector.HexChars
    val bldr = new StringBuilder
    foreach { b =>
      bldr.append(HexChars(b >> 4 & 0x0f)).append(HexChars(b & 0x0f))
    }
    bldr.toString
  }

  /** Converts the contents of this byte vector to a binary string of `size * 8` digits.  */
  def toBin: String = {
    val bldr = new StringBuilder
    foreach { b =>
      var n = 7
      while (n >= 0) {
        bldr.append(if ((0x01 & (b >> n)) != 0) "1" else "0")
        n -= 1
      }
    }
    bldr.toString
  }

  def leftShift(n: Int): ByteVector =
    BitVector(this).leftShift(n).toByteVector

  def rightShift(n: Int, signExtension: Boolean): ByteVector =
    BitVector(this).rightShift(n, signExtension).toByteVector

  def not: ByteVector = mapI { ~_ }

  def and(other: ByteVector): ByteVector =
    zipWithI(other)(_ & _)

  def or(other: ByteVector): ByteVector =
    zipWithI(other)(_ | _)

  def xor(other: ByteVector): ByteVector =
    zipWithI(other)(_ ^ _)
}

object ByteVector {

  private val HexChars: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  val empty: ByteVector = StandardByteVector(Vector.empty)

  def apply[A: Integral](bytes: A*): ByteVector = {
    val integral = implicitly[Integral[A]]
    StandardByteVector(bytes.map { i => integral.toInt(i).toByte }.toVector)
  }

  def apply(bytes: Vector[Byte]): ByteVector = StandardByteVector(bytes)

  def apply(bytes: Array[Byte]): ByteVector = StandardByteVector(bytes.toVector)

  def apply(buffer: ByteBuffer): ByteVector = {
    val arr = Array.ofDim[Byte](buffer.remaining)
    buffer.get(arr)
    apply(arr)
  }

  def fill[A: Integral](size: Int)(b: A): ByteVector = {
    val integral = implicitly[Integral[A]]
    StandardByteVector(Vector.fill[Byte](size)(integral.toInt(b).toByte))
  }

  def low(size: Int): ByteVector = fill(size)(0)
  def high(size: Int): ByteVector = fill(size)(0xff)

  def fromHex(str: String): String \/ ByteVector = {
    val withoutPrefix = if (str startsWith "0x") str.substring(2) else str
    withoutPrefix.replaceAll("\\s", "").sliding(2, 2).zipWithIndex.toVector.map { case (octet, idx) =>
      try java.lang.Integer.valueOf(octet, 16).toByte.right
      catch { case e: NumberFormatException => s"Invalid octet '$octet' at position ${idx * 2}".left }
    }.sequenceU.map { v => ByteVector(v) }
  }

  def fromValidHex(str: String): ByteVector =
    fromHex(str) valueOr { msg => throw new IllegalArgumentException(msg) }

  implicit val monoidInstance: Monoid[ByteVector] = new Monoid[ByteVector] {
    override def zero: ByteVector = ByteVector.empty
    override def append(x: ByteVector, y: => ByteVector) = x ++ y
  }

}
