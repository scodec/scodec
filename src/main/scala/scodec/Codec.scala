package scodec

import scala.language.implicitConversions

import scalaz.{ \/, InvariantFunctor, Monoid, StateT }
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
   * `g` maps to `None`.
   * @group combinators
   */
  final def pxmap[B](f: A => B, g: B => Option[A]): Codec[B] = new Codec[B] {
    def encode(b: B): String \/ BitVector = g(b).map(self.encode).getOrElse(left(s"extraction failure: $b"))
    def decode(buffer: BitVector): String \/ (BitVector, B) = self.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  /**
   * Returns a new codec that encodes/decodes a value of type `B` by using an isomorphism between `A` and `B`.
   *
   * The isomorphism is provided by the implicit `CodecAsAux` instance.
   *
   * Typically used when `B` is a case class and `A` is an `HList` with the same shape as the elements of `B`.
   *
   * @group hlist
   */
  final def as[B](implicit as: CodecAsAux[B, A]): Codec[B] = as(this)

  /**
   * Lifts this codec in to a codec of a singleton hlist.
   *
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
   * Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
   * decodes a unit followed by a `B` and discards the decoded unit.
   *
   * @group tuple
   */
  final def dropLeft[B](codecB: Codec[B])(implicit ev: Unit =:= A): Codec[B] =
    pairedWith(codecB).xmap[B]({ case (a, b) => b }, b => (ev(()), b))

  /**
   * Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
   * decodes a unit followed by a `B` and discards the decoded unit.
   *
   * Operator alias of [[dropLeft]].
   * @group tuple
   */
  final def ~>[B](codecB: Codec[B])(implicit ev: Unit =:= A): Codec[B] = dropLeft(codecB)

  /**
   * Assuming `B` is `Unit`, creates a `Codec[A]` that: encodes the `A` followed by a unit;
   * decodes an `A` followed by a unit and discards the decoded unit.
   *
   * @group tuple
   */
  final def dropRight[B](codecB: Codec[B])(implicit ev: Unit =:= B): Codec[A] =
    pairedWith(codecB).xmap[A]({ case (a, b) => a }, a => (a, ev(())))

  /**
   * Assuming `B` is `Unit`, creates a `Codec[A]` that: encodes the `A` followed by a unit;
   * decodes an `A` followed by a unit and discards the decoded unit.
   *
   * Operator alias of [[dropRight]].
   * @group tuple
   */
  final def <~[B](codecB: Codec[B])(implicit ev: Unit =:= B): Codec[A] = dropRight(codecB)

  /**
   * Converts this to a `Codec[Unit]` that encodes using the specified zero value and
   * decodes a unit value when this codec decodes an `A` successfully.
   *
   * @group combinators
   */
  final def unit(zero: A): Codec[Unit] = xmap[Unit](_ => (), _ => zero)

  /**
   * Converts this to a `Codec[Unit]` that encodes using the zero value of the implicitly
   * available `Monoid[A]` and decodes a unit value when this codec decodes an `A` successfully.
   *
   * @group combinators
   */
  final def unitM(implicit ma: Monoid[A]): Codec[Unit] = unit(ma.zero)

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

/**
 * Typeclass that witnesses that a `Codec[A]` can be xmapped in to a `Codec[B]`.
 *
 * Implicit instances (forward and reverse) are provided between:
 *  - case classes and `HList`s of compatible shapes
 *  - singleton case classes and values of proper types
 *
 * Credit: Miles Sabin
 */
@annotation.implicitNotFound("""Could not prove that ${B} can be converted to/from ${A}.
Proof is automatically available between case classes and HLists that have the same shape, as singleton case classes and values of matching types.""")
abstract class CodecAsAux[B, A] {
  def apply(ca: Codec[A]): Codec[B]
}

/** Companion for [[CodecAsAux]]. */
object CodecAsAux {

  /** Provides a `CodecAsAux[B, A]` for case class `B` and HList `A`. */
  implicit def mkAs[B, Repr, A](implicit gen: Generic.Aux[B, Repr], aToR: A =:= Repr, rToA: Repr =:= A): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.from(a)
      val to: B => A = b => gen.to(b)
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAsAux[B, A]` for HList `B` and case class `A`. */
  implicit def mkAsReverse[B, Repr, A](implicit gen: Generic.Aux[A, Repr], bToR: B =:= Repr, rToB: Repr =:= B): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.to(a)
      val to: B => A = b => gen.from(b)
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAsAux[B, A]` for singleton case class `B` and value `A`. */
  implicit def mkAsSingleton[B, Repr, A](implicit gen: Generic.Aux[B, Repr], aToR: (A :: HNil) =:= Repr, rToA: Repr =:= (A :: HNil)): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.from(a :: HNil)
      val to: B => A = b => gen.to(b).head
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAsAux[B, A]` for value `B` and singleton case class `A`. */
  implicit def mkAsSingletonReverse[B, Repr, A](implicit gen: Generic.Aux[A, Repr], bToR: (B :: HNil) =:= Repr, rToB: Repr =:= (B :: HNil)): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.to(a).head
      val to: B => A = b => gen.from(b :: HNil)
      ca.xmap(from, to)
    }
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

  val invariantFunctorInstance: InvariantFunctor[Codec] = new InvariantFunctor[Codec] {
    def xmap[A, B](c: Codec[A], f: A => B, g: B => A) = c.xmap(f, g)
  }
}
