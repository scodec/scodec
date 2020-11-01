/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec

/**
  * Bounds the size, in bits, of the binary encoding of a codec -- i.e., it provides a lower bound and an upper bound on the size
  * of bit vectors returned as a result of encoding.
  *
  * @param lowerBound Minimum number of bits
  * @param upperBound Maximum number of bits
  */
case class SizeBound(lowerBound: Long, upperBound: Option[Long]) {
  require(lowerBound >= 0)
  require(upperBound.getOrElse(0L) >= 0)
  require(upperBound.getOrElse(lowerBound) >= lowerBound)

  /**
    * Returns the exact size of the encoded bits. Defined when the lower bound is equal to the upper bound.
    */
  def exact: Option[Long] = upperBound match {
    case Some(u) if lowerBound == u => upperBound
    case _                          => None
  }

  /** Returns a new bound with the upper bound removed. */
  def atLeast: SizeBound = copy(upperBound = None)

  /** Returns a new bound with the lower bound reset to 0. */
  def atMost: SizeBound = copy(lowerBound = 0)

  /** Adds the specified size bound to this size bound. */
  def +(that: SizeBound): SizeBound = combine(that)(_ + _, _ + _)

  /** Multiplies this size bound by the specified scalar. */
  def *(n: Long): SizeBound =
    SizeBound(lowerBound = lowerBound * n, upperBound = upperBound.map(_ * n))

  /** ORs the specified size bound with this size bound, resulting in a new bound which has the minimum lower bound and maximum upper bound. */
  def |(that: SizeBound): SizeBound = combine(that)(_.min(_), _.max(_))

  private def combine(
      that: SizeBound
  )(lop: (Long, Long) => Long, uop: (Long, Long) => Long): SizeBound = SizeBound(
    lowerBound = lop(lowerBound, that.lowerBound),
    upperBound = for { x <- upperBound; y <- that.upperBound } yield uop(x, y)
  )

  override def toString = upperBound match {
    case Some(u) if lowerBound == u => u.toString
    case Some(u)                    => s"[$lowerBound, $u]"
    case None                       => s"[$lowerBound, ∞)"
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
  def choice(bounds: Iterable[SizeBound]): SizeBound =
    if (bounds.isEmpty) SizeBound.exact(0) else bounds.reduce(_ | _)
}
