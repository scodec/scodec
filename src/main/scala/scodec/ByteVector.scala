package scodec

import scala.collection.{IndexedSeqLike, IndexedSeqOptimized}

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

  def toHexadecimal: String

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
}
