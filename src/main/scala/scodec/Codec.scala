package scodec

import scalaz.{\/, Monoid, StateT}
import shapeless._


/**
 * Supports encoding a value of type `A` to a `BitVector` and decoding a `BitVector` to a value of `A`.
 *
 * Not every value of `A` can be encoded to a bit vector and similarly, not every bit vector can be decoded to a value
 * of type `A`. Hence, both encode and decode return either an error or the result. Furthermore, decode returns the
 * remaining bits in the bit vector that it did not use in decoding.
 *
 * Note: the decode function can be lifted to a state action via `StateT[Error \/ ?, BitVector, A]`. This type alias
 * and associated constructor is provided by `Codec.DecodingContext`.
 */
trait Codec[A] {

  /** Attempts to encode the specified value in to a bit vector. */
  def encode(a: A): Error \/ BitVector

  /** Attempts to decode a value of type `A` from the specified bit vector. */
  def decode(bits: BitVector): Error \/ (BitVector, A)

  /** Maps to a codec of type `B`. */
  final def xmap[B](f: A => B, g: B => A): Codec[B] = Codec.xmap(this)(f, g)

  /** Returns a new codec that encodes/decodes a value of type `B` by using an iso between `A` and `B`. */
  // TODO: results in: could not find implicit value for parameter gen: shapeless.Generic.Aux[Bar,Int]
  final def as[B](implicit gen: Generic.Aux[B, A]): Codec[B] = Codec.xmap(this)(gen.from, gen.to(_))

  /** Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`. */
  final def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = Codec.flatZip(this)(f)

  /** Operator alias for `flatZip`. */
  final def >>~[B](f: A => Codec[B]): Codec[(A, B)] = flatZip(f)

  /** Lifts this codec in to a codec of a singleton hlist, which allows easy binding to case classes of one argument. */
  final def hlist: Codec[A :: HNil] = Codec.xmap(this)(_ :: HNil, _.head)
}

object Codec {

  /** Creates a codec from encoder and decoder functions. */
  def apply[A](encoder: A => Error \/ BitVector, decoder: BitVector => Error \/ (BitVector, A)): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder(a)
    override def decode(bits: BitVector) = decoder(bits)
  }

  /** Gets the implicitly available codec for type `A`. */
  def apply[A: Codec]: Codec[A] = implicitly[Codec[A]]

  /** Alias for state/either transformer that simplifies calling decode on a series of codecs, wiring the remaining bit vector of each in to the next entry. */
  type DecodingContext[+A] = StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A]

  /** Provides constructors for `DecodingContext`. */
  object DecodingContext {
    def apply[A](f: BitVector => Error \/ (BitVector, A)): DecodingContext[A] =
      StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A](f)

    def liftE[A](e: Error \/ A): DecodingContext[A] =
      apply { bv => e map { a => (bv, a) } }

    def monadState = StateT.stateTMonadState[BitVector, ({type λ[+a] = Error \/ a})#λ]
  }

  /** Encodes the specified value to a bit vector. */
  def encode[A](codec: Codec[A], a: A): Error \/ BitVector =
    codec encode a

  /** Encodes the specified value to a bit vector using an implicitly available codec. */
  def encode[A: Codec](a: A): Error \/ BitVector =
    encode(Codec[A], a)

  /** Decodes the specified bit vector using the specified codec and discards the remaining bits. */
  def decode[A](codec: Codec[A], buffer: BitVector): Error \/ A =
    codec decode buffer map { case (rest, result) => result }

  /** Decodes the specified buffer in to a value of type `A` using an implicitly available codec and discards the remaining bits. */
  def decode[A: Codec](buffer: BitVector): Error \/ A =
    decode(Codec[A], buffer)

  def decodeAll[A: Codec, B: Monoid](buffer: BitVector)(f: A => B): (Option[Error], B) = {
    val codec = Codec[A]
    var remaining = buffer
    var acc = Monoid[B].zero
    while (remaining.nonEmpty) {
      codec.decode(remaining).fold(
        { err => return (Some(err), acc) },
        { case (newRemaining, a) =>
            remaining = newRemaining
            acc = Monoid[B].append(acc, f(a))
        }
      )
    }
    (None, acc)
  }

  /** Maps a `Codec[A]` in to a `Codec[B]`. */
  def xmap[A, B](codec: Codec[A])(f: A => B, g: B => A): Codec[B] = new Codec[B] {
    def encode(b: B): Error \/ BitVector = codec.encode(g(b))
    def decode(buffer: BitVector): Error \/ (BitVector, B) = codec.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  def dropLeft[A: Monoid, B](codecA: Codec[A], codecB: Codec[B]): Codec[B] =
    xmap[(A, B), B](new TupleCodec(codecA, codecB))({ case (a, b) => b }, b => (Monoid[A].zero, b))

  def dropRight[A, B: Monoid](codecA: Codec[A], codecB: Codec[B]): Codec[A] =
    xmap[(A, B), A](new TupleCodec(codecA, codecB))({ case (a, b) => a }, a => (a, Monoid[B].zero))

  def encodeBoth[A, B](codecA: Codec[A], codecB: Codec[B])(a: A, b: B): Error \/ BitVector = for {
    encA <- codecA.encode(a)
    encB <- codecB.encode(b)
  } yield encA ++ encB

  def decodeBoth[A, B](codecA: Codec[A], codecB: Codec[B])(buffer: BitVector): Error \/ (BitVector, (A, B)) = (for {
    a <- DecodingContext(codecA.decode)
    b <- DecodingContext(codecB.decode)
  } yield (a, b)).run(buffer)

  def flatZip[A, B](codecA: Codec[A])(f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
    def encode(t: (A, B)) = encodeBoth(codecA, f(t._1))(t._1, t._2)
    def decode(buffer: BitVector) = (for {
      a <- DecodingContext(codecA.decode)
      b <- DecodingContext(f(a).decode)
    } yield (a, b)).run(buffer)
  }
}
