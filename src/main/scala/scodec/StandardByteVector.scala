package scodec

import scala.collection.mutable.Builder


object StandardByteVector {
  def apply(bytes: Vector[Byte]): StandardByteVector = new StandardByteVector(bytes)
}

/**
 * `ByteVector` implemented with `scala.collection.immutable.Vector[Byte]`.
 *
 * Note: This implementation has good algorithmic performance characteristics but results
 * in lots of boxing/unboxing of bytes.
 */
class StandardByteVector(private val bytes: Vector[Byte]) extends ByteVector {

  override def length = bytes.length

  override def apply(idx: Int): Byte = bytes(idx)

  override def lift(idx: Int): Option[Byte] = bytes.lift(idx)

  override def updated(idx: Int, b: Byte): StandardByteVector = StandardByteVector(bytes.updated(idx, b))

  override def +:(byte: Byte): ByteVector = StandardByteVector(byte +: bytes)

  override def :+(byte: Byte): ByteVector = StandardByteVector(bytes :+ byte)

  override def ++(other: ByteVector): ByteVector = { other match {
    case otherVector: StandardByteVector => StandardByteVector(bytes ++ otherVector.bytes)
    case _ => StandardByteVector(bytes ++ other.toIterable)
  }}

  override def drop(n: Int): ByteVector = StandardByteVector(bytes drop n)

  override def dropRight(n: Int): ByteVector = StandardByteVector(bytes dropRight n)

  override def take(n: Int): ByteVector = StandardByteVector(bytes take n)

  override def takeRight(n: Int): ByteVector = StandardByteVector(bytes takeRight n)

  override def slice(from: Int, until: Int): ByteVector = StandardByteVector(bytes.slice(from, until))

  override def map(f: Byte => Byte): ByteVector = StandardByteVector(bytes map f)

  override def zipWith(other: ByteVector)(op: (Byte, Byte) => Byte): ByteVector =
    StandardByteVector((bytes zip other.toIterable) map op.tupled)

  override def seq: IndexedSeq[Byte] = bytes.seq

  override def iterator: Iterator[Byte] = bytes.iterator

  override def toArray: Array[Byte] = bytes.toArray

  override protected[this] def thisCollection = bytes

  override def toString: String = s"ByteVector(0x${toHex})"

  override def hashCode: Int = bytes.hashCode

  override def equals(other: Any): Boolean = other match {
    case o: StandardByteVector => bytes == o.bytes
    case o: ByteVector => bytes == o.toVector
    case _ => false
  }

  override protected[this] def newBuilder: Builder[Byte, ByteVector] = {
    Vector.newBuilder.mapResult { v => ByteVector(v) }
  }
}
