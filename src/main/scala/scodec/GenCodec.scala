package scodec

import scalaz.{ \/, Profunctor }

import scodec.bits.BitVector

import shapeless.Lazy

/** Generalized codec that allows the type to encode to vary from the type to decode. */
trait GenCodec[-A, +B] extends Encoder[A] with Decoder[B] { self =>

  /** Converts this `GenCodec` to a `GenCodec[A, C]` using the supplied `B => C`. */
  override def map[C](f: B => C): GenCodec[A, C] = GenCodec(this, super.map(f))

  /** Converts this `GenCodec` to a `GenCodec[A, C]` using the supplied `B => Err \/ C`. */
  override def emap[C](f: B => Err \/ C): GenCodec[A, C] = GenCodec(this, super.emap(f))

  /** Converts this `GenCodec` to a `GenCodec[C, B]` using the supplied `C => A`. */
  override def contramap[C](f: C => A): GenCodec[C, B] = GenCodec(super.contramap(f), this)

  /**
   * Converts this `GenCodec` to a `GenCodec[C, B]` using the supplied partial
   * function from `C` to `A`. The encoding will fail for any `C` that
   * `f` maps to `None`.
   */
  override def pcontramap[C](f: C => Option[A]): GenCodec[C, B] = GenCodec(super.pcontramap(f), this)

  /** Converts this `GenCodec` to a `GenCodec[C, B]` using the supplied `C => Err \/ A`. */
  override def econtramap[C](f: C => Err \/ A): GenCodec[C, B] = GenCodec(super.econtramap(f), this)

  /**
   * Converts this generalized codec in to a non-generalized codec assuming `A` and `B` are the same type.
   * @group combinators
   */
  final def fuse[AA <: A, BB >: B](implicit ev: BB =:= AA): Codec[BB] = new Codec[BB] {
    def encode(c: BB) = self.encode(ev(c))
    def decode(bits: BitVector) = self.decode(bits)
  }

  /**
   * Converts this codec to a new codec that fails decoding if there are remaining bits.
   * @group combinators
   */
  override def complete: GenCodec[A, B] = GenCodec(this, super.complete)

  /**
   * Converts this codec to a new codec that compacts the encoded bit vector before returning it.
   * @group combinators
   */
  override def compact: GenCodec[A, B] = GenCodec(super.compact, this)
}

/**
 * Companion for [[GenCodec]].
 *
 * @groupname ctor Constructors
 * @groupprio ctor 1
 *
 * @groupname inst Typeclass Instances
 * @groupprio inst 3
 */
object GenCodec extends EncoderFunctions with DecoderFunctions {

  /**
   * Provides syntax for summoning a `GenCodec[A, B]` from implicit scope.
   * @group ctor
   */
  def apply[A, B](implicit gc: Lazy[GenCodec[A, B]]): GenCodec[A, B] = gc.value

  /**
   * Creates a generalized codec from an encoder and a decoder.
   * @group ctor
   */
  def apply[A, B](encoder: Encoder[A], decoder: Decoder[B]): GenCodec[A, B] = new GenCodec[A, B] {
    override def encode(a: A) = encoder.encode(a)
    override def decode(bits: BitVector) = decoder.decode(bits)
  }

  /**
   * Profunctor instance.
   * @group inst
   */
  implicit val profunctorInstance: Profunctor[GenCodec] = new Profunctor[GenCodec] {
    def mapfst[A, B, C](gc: GenCodec[A, B])(f: C => A) = gc contramap f
    def mapsnd[A, B, C](gc: GenCodec[A, B])(f: B => C) = gc map f
  }
}
