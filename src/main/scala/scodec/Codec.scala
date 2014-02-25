package scodec

import scalaz.{ \/, Monoid, StateT }
import \/.left
import shapeless._

import scodec.bits.BitVector

/**
 * Supports encoding a value of type `A` to a `BitVector` and decoding a `BitVector` to a value of `A`.
 *
 * Not every value of `A` can be encoded to a bit vector and similarly, not every bit vector can be decoded to a value
 * of type `A`. Hence, both encode and decode return either an error or the result. Furthermore, decode returns the
 * remaining bits in the bit vector that it did not use in decoding.
 *
 * Note: the decode function can be lifted to a state action via `StateT[String \/ ?, BitVector, A]`. This type alias
 * and associated constructor is provided by `[[DecodingContext]]`.
 *
 *
 * @groupname tuple Tuple Support
 * @groupprio tuple 11

 * @groupname hlist HList Support
 * @groupprio hlist 12
 */
trait Codec[A] extends GenCodec[A, A] { self =>

  /**
   * Maps to a codec of type `B` using two total functions, `A => B` and `B => A`.
   * @group combinators
   */
  final def xmap[B](f: A => B, g: B => A): Codec[B] = new Codec[B] {
    def encode(b: B): String \/ BitVector = self.encode(g(b))
    def decode(buffer: BitVector): String \/ (BitVector, B) = self.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  /**
   * Maps to a `codec` of type `B`, where there is a partial function
   * from `B` to `A`. The encoding will fail for any `B` that
   * `f` maps to `None`.
   * @group combinators
   */
  final def pxmap[B](f: A => B, g: B => Option[A]): Codec[B] = new Codec[B] {
    def encode(b: B): String \/ BitVector = g(b).map(self.encode).getOrElse(left(s"extraction failure: $b"))
    def decode(buffer: BitVector): String \/ (BitVector, B) = self.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  /**
   * Returns a new codec that encodes/decodes a value of type `B` by using an iso between `A` and `B`.
   * @group hlist
   */
  final def as[B](implicit iso: Iso[B, A]): Codec[B] = xmap(iso.from, iso.to)

  /**
   * Lifts this codec in to a codec of a singleton hlist, which allows easy binding to case classes of one argument.
   * @group hlist
   */
  final def hlist: Codec[A :: HNil] = xmap(_ :: HNil, _.head)

  /**
   * Creates a `Codec[(A, B)]` that first encodes/decodes an `A` followed by a `B`.
   * @group tuple
   */
  final def pairedWith[B](codecB: Codec[B]): Codec[(A, B)] = new codecs.TupleCodec(this, codecB)

  /**
   * Creates a `Codec[(A, B)]` that first encodes/decodes an `A` followed by a `B`.
   *
   * Operator alias for [[pairedWith]].
   * @group tuple
   */
  final def ~[B](codecB: Codec[B]): Codec[(A, B)] = pairedWith(codecB)

  /**
   * Creates a `Codec[B]` that: encodes the zero element of the `Monoid` of A` followed by a `B`;
   * decodes an `A` followed by a `B` and discards the decoded `A`.
   * @group tuple
   */
  final def dropLeft[B](codecB: Codec[B])(implicit ma: Monoid[A]): Codec[B] =
    pairedWith(codecB).xmap[B]({ case (a, b) => b }, b => (Monoid[A].zero, b))

  /**
   * Creates a `Codec[B]` that: encodes the zero element of the `Monoid` of A` followed by a `B`;
   * decodes an `A` followed by a `B` and discards the decoded `A`.
   *
   * Operator alias of [[dropLeft]].
   * @group tuple
   */
  final def ~>[B](codecB: Codec[B])(implicit ma: Monoid[A]): Codec[B] = dropLeft(codecB)

  /**
   * Creates a `Codec[A]` that: encodes an `A` followed by the zero element of the `Monoid` of `B`;
   * decodes an `A` followed by a `B` and discards the decoded `B`.
   * @group tuple
   */
  final def dropRight[B: Monoid](codecB: Codec[B]): Codec[A] =
    pairedWith(codecB).xmap[A]({ case (a, b) => a }, a => (a, Monoid[B].zero))

  /**
   * Creates a `Codec[A]` that: encodes an `A` followed by the zero element of the `Monoid` of `B`;
   * decodes an `A` followed by a `B` and discards the decoded `B`.
   *
   * Operator alias of [[dropRight]].
   * @group tuple
   */
  final def <~[B: Monoid](codecB: Codec[B]): Codec[A] = dropRight(codecB)

  /**
   * Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
   * @group tuple
   */
  final def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
    override def encode(t: (A, B)) = Codec.encodeBoth(self, f(t._1))(t._1, t._2)
    override def decode(buffer: BitVector) = (for {
      a <- DecodingContext(self.decode)
      b <- DecodingContext(f(a).decode)
    } yield (a, b)).run(buffer)
  }

  /**
   * Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
   * Operator alias for [[flatZip]].
   * @group tuple
   */
  final def >>~[B](f: A => Codec[B]): Codec[(A, B)] = flatZip(f)

  final override def complete: Codec[A] = Codec(this, super.complete)

  final override def compact: Codec[A] = Codec(super.compact, this)

  /**
   * Creates a new codec that is functionally equivalent to this codec but returns the specified string from `toString`.
   * @group combinators
   */
  final def withToString(str: String): Codec[A] = new Codec[A] {
    override def encode(a: A) = self.encode(a)
    override def decode(buffer: BitVector) = self.decode(buffer)
    override def toString = str
  }
}

/** Companion for [[Codec]]. */
object Codec extends EncoderFunctions with DecoderFunctions {

  /** Creates a codec from encoder and decoder functions. */
  def apply[A](encoder: A => String \/ BitVector, decoder: BitVector => String \/ (BitVector, A)): Codec[A] = new Codec[A] {
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

  // TODO Upon upgrade to Scalaz 7.1
  /*
  val invariantFunctorInstance: InvariantFunctor[A] = new InvariantFunctor[Codec] {
    ...
  }
  */
}
