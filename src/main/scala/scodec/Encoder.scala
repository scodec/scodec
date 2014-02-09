package scodec

import scalaz.{ \/, Contravariant, Corepresentable }

/** Supports encoding a value of type `A` to a `BitVector`. */
trait Encoder[-A] { self =>

  /**
   * Attempts to encode the specified value in to a bit vector.
   *
   * @param value value to encode
   * @param return error or binary encoding of the value
   */
  def encode(value: A): String \/ BitVector

  /** Converts this encoder to an `Encoder[B]` using the supplied `B => A`. */
  def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def encode(b: B) = self.encode(f(b))
  }
}

/** Provides functions for working with encoders. */
trait EncoderFunctions {

  /** Encodes the specified value to a bit vector. */
  final def encode[A](enc: Encoder[A], a: A): String \/ BitVector = enc encode a

  /** Encodes the specified value to a bit vector using an implicitly available encoder. */
  final def encode[A: Encoder](a: A): String \/ BitVector = encode(Encoder[A], a)

  /** Encodes the specified values, one after the other, to a bit vector using the specified encoders. */
  final def encodeBoth[A, B](encA: Encoder[A], encB: Encoder[B])(a: A, b: B): String \/ BitVector = for {
    encodedA <- encA.encode(a)
    encodedB <- encB.encode(b)
  } yield encodedA ++ encodedB
}

/** Companion for [[Encoder]]. */
object Encoder extends EncoderFunctions {

  /** Provides syntaax for summoning an `Encoder[A]` from implicit scope. */
  def apply[A](implicit enc: Encoder[A]): Encoder[A] = enc

  def instance[A](f: A => String \/ BitVector): Encoder[A] = new Encoder[A] {
    def encode(value: A) = f(value)
  }

  implicit val contravariantInstance: Contravariant[Encoder] = new Contravariant[Encoder] {
    def contramap[A, B](e: Encoder[A])(f: B => A) = e contramap f
  }

  implicit val corepresentableInstance: Corepresentable[Encoder, String \/ BitVector] = new Corepresentable[Encoder, String \/ BitVector] {
    def corep[A](f: A => String \/ BitVector): Encoder[A] = Encoder.instance(f)
    def uncorep[A](f: Encoder[A]): A => String \/ BitVector = f.encode
  }
}
