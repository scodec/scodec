package scodec

/** A `ByteVector` built off a function from `Int => Byte` and an offset. */
class SliceByteVector(at: Int => Byte, offset: Int, size: Int) extends ByteVector { self =>

  override def apply(idx: Int): Byte = at(idx + offset)

  def lift(idx: Int): Option[Byte] =
    if (idx < size) Some(apply(idx))
    else None

  protected def toStandardByteVector = ByteVector(Vector.empty ++ toArray)

  override def drop(n: Int): ByteVector = new SliceByteVector(at, offset + n, (size-n) max 0)
  override def take(n: Int): ByteVector = new SliceByteVector(at, offset, size min n)

  // these just convert to StandardByteVector
  def :+(b: Byte): ByteVector = toStandardByteVector :+ b
  def +:(b: Byte): ByteVector = b +: toStandardByteVector
  def ++(vec: ByteVector): ByteVector = toStandardByteVector ++ vec
  def updated(idx: Int, b: Byte): ByteVector = toStandardByteVector.updated(idx, b)

  def zipWith(vec: ByteVector)(f: (Byte,Byte) => Byte): ByteVector =
    new SliceByteVector((i: Int) => f(apply(i), vec(i)), 0, size min vec.size)

  def length = size

  def seq: IndexedSeq[Byte] = toArray

  def toArray: Array[Byte] = {
    val buf = new Array[Byte](size)
    var i = offset
    while (i < size) {
      buf(i) = apply(i)
      i += 1
    }
    buf
  }

  def map(f: Byte => Byte): ByteVector = ByteVector(toArray).map(f)

  protected def newBuilder = new scala.collection.mutable.Builder[Byte,ByteVector] {
    var buf = new Array[Byte](self.size)
    var i = 0
    def +=(b: Byte) = { buf(i) = b; i += 1; this }
    def clear() = { buf = new Array[Byte](self.size); i = 0 }
    def result() = new SliceByteVector(idx => buf(idx), 0, i)
  }
}
