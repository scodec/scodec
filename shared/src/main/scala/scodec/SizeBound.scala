package scodec

/**
 * Bounds the size, in bits, of the binary encoding of a codec -- i.e., it provides a lower bound and an upper bound on the size
 * of bit vectors returned as a result of encoding.
 *
 * @param lowerBound Minimum number of bits
 * @param upperBound Maximum number of bits
 */
final case class SizeBound(lowerBound: Long, upperBound: Option[Long]) {
  require(lowerBound >= 0)
  require(upperBound.getOrElse(0L) >= 0)
  require(upperBound.getOrElse(lowerBound) >= lowerBound)

  /**
   * Returns the exact size of the encoded bits. Defined when the lower bound is equal to the upper bound.
   */
  def exact: Option[Long] = upperBound match {
    case Some(u) if lowerBound == u => upperBound
    case other => None
  }

  /** Returns a new bound with the upper bound removed. */
  def atLeast: SizeBound = copy(upperBound = None)

  /** Returns a new bound with the lower bound reset to 0. */
  def atMost: SizeBound = copy(lowerBound = 0)

  /** Adds the specified size bound to this size bound. */
  def +(that: SizeBound): SizeBound = combine(that)(_ + _, _ + _)

  /** Multiplies this size bound by the specified scalar. */
  def *(n: Long): SizeBound = SizeBound(lowerBound = lowerBound * n, upperBound = upperBound.map { _ * n })

  /** ORs the specified size bound with this size bound, resulting in a new bound which has the minimum lower bound and maximum upper bound. */
  def |(that: SizeBound): SizeBound = combine(that)(_ min _, _ max _)

  private def combine(that: SizeBound)(lop: (Long, Long) => Long, uop: (Long, Long) => Long): SizeBound = SizeBound(
    lowerBound = lop(lowerBound, that.lowerBound),
    upperBound = for { x <- upperBound; y <- that.upperBound } yield uop(x, y)
  )

  override def toString = upperBound match {
    case Some(u) if lowerBound == u => u.toString
    case Some(u) => s"[$lowerBound, $u]"
    case None => s"[$lowerBound, ∞)"
  }
}

/** Companion for [[SizeBound]]. */
object SizeBound {

  /** Creates a bound for an exact size. */
  def exact(size: Long): SizeBound = SizeBound(size, Some(size))

  /** Creates a bound with the specified lower bound and no upper bound. */
  def atLeast(size: Long): SizeBound = SizeBound(size, None)

  /** Creates a bound with a lower bound of 0 and the specified upper bound. */
  def atMost(size: Long): SizeBound = SizeBound(0, Some(size))

  /** Creates a bound with the specified lower and upper bounds. */
  def bounded(lower: Long, upper: Long): SizeBound = SizeBound(lower, Some(upper))

  /** Bound that indicates nothing is known about the size of encoded vectors. */
  val unknown: SizeBound = atLeast(0)

  /** Returns the union of all of the specified bounds, or an exact 0 size if the specified collection is empty. */
  def choice(bounds: collection.GenTraversableOnce[SizeBound]): SizeBound =
    if (bounds.isEmpty) SizeBound.exact(0) else bounds.toIterator.reduce(_ | _)
}
