package scodec

import scalaz.{ \/, Monoid, StateT }
import shapeless._


/**
 * Supports encoding a value of type `A` to a `BitVector` and decoding a `BitVector` to a value of `A`.
 *
 * Not every value of `A` can be encoded to a bit vector and similarly, not every bit vector can be decoded to a value
 * of type `A`. Hence, both encode and decode return either an error or the result. Furthermore, decode returns the
 * remaining bits in the bit vector that it did not use in decoding.
 *
 * Note: the decode function can be lifted to a state action via `StateT[Error \/ ?, BitVector, A]`. This type alias
 * and associated constructor is provided by `DecodingContext`.
 */
trait Codec[A] extends GenCodec[A, A] {

  /** Maps to a codec of type `B`. */
  final def xmap[B](f: A => B, g: B => A): Codec[B] = Codec.xmap(this)(f, g)

  /** Returns a new codec that encodes/decodes a value of type `B` by using an iso between `A` and `B`. */
  final def as[B](implicit iso: Iso[B, A]): Codec[B] = Codec.xmap(this)(iso.from, iso.to)

  /** Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`. */
  final def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = Codec.flatZip(this)(f)

  /** Operator alias for `flatZip`. */
  final def >>~[B](f: A => Codec[B]): Codec[(A, B)] = flatZip(f)

  /** Lifts this codec in to a codec of a singleton hlist, which allows easy binding to case classes of one argument. */
  final def hlist: Codec[A :: HNil] = Codec.xmap(this)(_ :: HNil, _.head)

  /** Creates a `Codec[(A, B)]` that first encodes/decodes an `A` followed by a `B`. */
  final def ~[B](codecB: Codec[B]): Codec[(A, B)] = new codecs.TupleCodec(this, codecB)

  /**
   * Creates a `Codec[A]` that:
   * - Encodes an `A` followed by the zero element of the `Monoid` of `B`.
   * - Decodes an `A` followed by a `B` and discards the decoded `B`.
   *
   * Operator alias for `Codec.dropRight`.
   */
  final def <~[B: Monoid](codecB: Codec[B]): Codec[A] = Codec.dropRight(this, codecB)

  /**
   * Creates a `Codec[B]` that:
   * - Encodes the zero element of the `Monoid` of A` followed by a `B`.
   * - Decodes an `A` followed by a `B` and discards the decoded `A`.
   *
   * Operator alias for `Codec.dropLeft`.
   */
  final def ~>[B](codecB: Codec[B])(implicit ma: Monoid[A]): Codec[B] = Codec.dropLeft(this, codecB)

  /** Creates a new codec that is functionally equivalent to this codec but returns the specified string from `toString`. */
  final def withToString(str: String): Codec[A] = Codec.withToString(this)(str)
}

/** Companion for [[Codec]]. */
object Codec extends EncoderFunctions with DecoderFunctions {

  /** Creates a codec from encoder and decoder functions. */
  def apply[A](encoder: A => Error \/ BitVector, decoder: BitVector => Error \/ (BitVector, A)): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder(a)
    override def decode(bits: BitVector) = decoder(bits)
  }

  /** Creates a codec from an encoder and a decoder. */
  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder.encode(a)
    override def decode(bits: BitVector) = decoder.decode(bits)
  }

  /** Gets the implicitly available codec for type `A`. */
  def apply[A: Codec]: Codec[A] = implicitly[Codec[A]]

  /** Maps a `Codec[A]` in to a `Codec[B]`. */
  def xmap[A, B](codec: Codec[A])(f: A => B, g: B => A): Codec[B] = new Codec[B] {
    def encode(b: B): Error \/ BitVector = codec.encode(g(b))
    def decode(buffer: BitVector): Error \/ (BitVector, B) = codec.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  /**
   * Creates a `Codec[B]` that:
   * - Encodes the zero element of the `Monoid` of A` followed by a `B`.
   * - Decodes an `A` followed by a `B` and discards the decoded `A`.
   */
  def dropLeft[A: Monoid, B](codecA: Codec[A], codecB: Codec[B]): Codec[B] =
    xmap[(A, B), B](new codecs.TupleCodec(codecA, codecB))({ case (a, b) => b }, b => (Monoid[A].zero, b))

  /**
   * Creates a `Codec[A]` that:
   * - Encodes an `A` followed by the zero element of the `Monoid` of `B`.
   * - Decodes an `A` followed by a `B` and discards the decoded `B`.
   */
  def dropRight[A, B: Monoid](codecA: Codec[A], codecB: Codec[B]): Codec[A] =
    xmap[(A, B), A](new codecs.TupleCodec(codecA, codecB))({ case (a, b) => a }, a => (a, Monoid[B].zero))

  def flatZip[A, B](codecA: Codec[A])(f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
    override def encode(t: (A, B)) = encodeBoth(codecA, f(t._1))(t._1, t._2)
    override def decode(buffer: BitVector) = (for {
      a <- DecodingContext(codecA.decode)
      b <- DecodingContext(f(a).decode)
    } yield (a, b)).run(buffer)
  }

  def withToString[A](codec: Codec[A])(str: String): Codec[A] = new Codec[A] {
    override def encode(a: A) = codec.encode(a)
    override def decode(buffer: BitVector) = codec.decode(buffer)
    override def toString = str
  }

  // TODO Upon upgrade to Scalaz 7.1
  /*
  val invariantFunctorInstance: InvariantFunctor[A] = new InvariantFunctor[Codec] {
    ...
  }
  */
}
