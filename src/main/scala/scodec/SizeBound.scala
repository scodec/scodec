package scodec

/**
 * Bounds the size, in bits, of the binary encoding of a codec.
 */
final case class SizeBound private (lowerBound: Long, upperBound: Option[Long]) {
  require(lowerBound >= 0)
  require(upperBound.getOrElse(0L) >= 0)

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

object SizeBound {
  def exact(size: Long): SizeBound = SizeBound(size, Some(size))
  def atLeast(size: Long): SizeBound = SizeBound(size, None)
  def atMost(size: Long): SizeBound = SizeBound(0, Some(size))
  def bounded(lower: Long, upper: Long): SizeBound = SizeBound(lower, Some(upper))
  val unknown: SizeBound = atLeast(0)

  def choice(bounds: collection.GenTraversableOnce[SizeBound]): SizeBound =
    if (bounds.isEmpty) SizeBound.exact(0) else bounds.reduce(_ | _)
}
