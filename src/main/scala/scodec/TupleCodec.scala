package scodec

import Codec.DecodingContext


class TupleCodec[A, B](codecA: Codec[A], codecB: Codec[B]) extends Codec[(A, B)] {

  override def encode(t: (A, B)) = for {
    a <- codecA.encode(t._1)
    b <- codecB.encode(t._2)
  } yield a ++ b

  override def decode(buffer: BitVector) = (for {
    a <- DecodingContext(codecA.decode)
    b <- DecodingContext(codecB.decode)
  } yield (a, b)).run(buffer)

}

/**
 * Provides support for combining codecs via the `~` operator.
 *
 * For example: {{{
   val codec: Codec[Int ~ Int ~ Int] = uint8 ~ uint8 ~ uint8}}}
 *
 * Codecs generated with `~` result in left nested tuples. These left nested tuples can
 * be pulled back apart by pattern matching with `~`. For example: {{{
  Codec.decode(uint8 ~ uint8 ~ uint8, bytes) map { case a ~ b ~ c => a + b + c }
 }}}
 *
 * Alternatively, a function of N arguments can be lifted to a function of left-nested tuples. For example: {{{
  val add3 = (_: Int) + (_: Int) + (_: Int)
  Codec.decode(uint8 ~ uint8 ~ uint8, bytes) map add3
 }}}
 *
 * Similarly, a left nested tuple can be created with the `~` operator. This is useful when creating the tuple structure
 * to pass to encode. For example: {{{
  (uint8 ~ uint8 ~ uint8).encode(1 ~ 2 ~ 3)
 }}}
 *
 * Note: this design is heavily based on Scala's parser combinator library and the syntax it provides.
 */
trait TupleCodecSyntax extends TupleCodecSyntax0 {

  /** Type alias for Tuple2 in order to allow left nested tuples to be written as A ~ B ~ C ~ .... */
  type ~[+A, +B] = (A, B)

  /** Allows two codecs to be combined in to a single codec that produces a tuple. */
  implicit class CodecEnrichedWithTuplingSupport[A](val codecA: Codec[A]) {
    def ~[B](codecB: Codec[B]): Codec[(A, B)] = new TupleCodec(codecA, codecB)
  }

  /** Extractor that allows pattern matching on the tuples created by tupling codecs. */
  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = Some(t)
  }

  implicit def liftF2ToNestedTupleF[A, B, X](fn: (A, B) => X): ((A, B)) => X =
    fn.tupled
  implicit def liftF3ToNestedTupleF[A, B, C, X](fn: (A, B, C) => X): (((A, B), C)) => X = {
    case a ~ b ~ c => fn(a, b, c)
  }
  implicit def liftF4ToNestedTupleF[A, B, C, D, X](fn: (A, B, C, D) => X): ((((A, B), C), D)) => X = {
    case a ~ b ~ c ~ d => fn(a, b, c, d)
  }
  implicit def liftF5ToNestedTupleF[A, B, C, D, E, X](fn: (A, B, C, D, E) => X): (((((A, B), C), D), E)) => X = {
    case a ~ b ~ c ~ d ~ e => fn(a, b, c, d, e)
  }
  implicit def liftF6ToNestedTupleF[A, B, C, D, E, F, X](fn: (A, B, C, D, E, F) => X): ((((((A, B), C), D), E), F)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f => fn(a, b, c, d, e, f)
  }
  implicit def liftF7ToNestedTupleF[A, B, C, D, E, F, G, X](fn: (A, B, C, D, E, F, G) => X): (((((((A, B), C), D), E), F), G)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f ~ g => fn(a, b, c, d, e, f, g)
  }
  implicit def liftF8ToNestedTupleF[A, B, C, D, E, F, G, H, X](fn: (A, B, C, D, E, F, G, H) => X): ((((((((A, B), C), D), E), F), G), H)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h => fn(a, b, c, d, e, f, g, h)
  }
}

/** Low priority implicits related to tupling. */
trait TupleCodecSyntax0 {

  /** Allows creation of left nested tuples by successive usage of `~` operator. */
  implicit class ValueEnrichedWithTuplingSupport[A](val a: A) {
    def ~[B](b: B): (A, B) = (a, b)
  }
}

object TupleCodecSyntax extends TupleCodecSyntax
