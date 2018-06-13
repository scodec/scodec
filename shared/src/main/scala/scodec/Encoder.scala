package scodec

import scala.annotation.unchecked.uncheckedVariance

import scodec.bits.BitVector

import shapeless.Lazy

/**
 * Supports encoding a value of type `A` to a `BitVector`.
 *
 * @groupname primary Primary Members
 * @groupprio primary 0
 *
 * @groupname combinators Basic Combinators
 * @groupprio combinators 10
 *
 * @groupname coproduct Coproduct Support
 * @groupprio coproduct 13
 */
trait Encoder[-A] { self =>

  /**
   * Attempts to encode the specified value in to a bit vector.
   *
   * @param value value to encode
   * @return error or binary encoding of the value
   * @group primary
   */
  def encode(value: A): Attempt[BitVector]

  /**
   * Provides a bound on the size of successfully encoded values.
   *
   * @group primary
   */
  def sizeBound: SizeBound


  /**
   * Converts this encoder to an `Encoder[B]` using the supplied `B => A`.
   * @group combinators
   */
  def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def sizeBound = self.sizeBound
    def encode(b: B) = self.encode(f(b))
  }

  /**
   * Converts this encoder to an `Encoder[B]` using the supplied partial
   * function from `B` to `A`. The encoding will fail for any `B` that
   * `f` maps to `None`.
   * @group combinators
   */
  def pcontramap[B](f: B => Option[A]): Encoder[B] = new Encoder[B] {
    def sizeBound = self.sizeBound
    def encode(b: B): Attempt[BitVector] =
      f(b).map(self.encode).getOrElse(Attempt.failure(Err(s"widening failed: $b")))
  }

  /**
   * Converts this encoder to an `Encoder[B]` using the supplied `B => Attempt[A]`.
   * @group combinators
   */
  def econtramap[B](f: B => Attempt[A]): Encoder[B] = new Encoder[B] {
    def sizeBound = self.sizeBound
    def encode(b: B) = f(b) flatMap self.encode
  }

  /**
   * Converts this encoder to a new encoder that compacts the generated bit vector before returning it
   * @group combinators
   */
  def compact: Encoder[A] = new Encoder[A] {
    def sizeBound = self.sizeBound
    def encode(a: A) = self.encode(a).map { _.compact }
  }

  /**
   * Gets this as an `Encoder`.
   * @group combinators
   */
  def asEncoder: Encoder[A] = this

  /**
   * Converts this to a codec that fails decoding with an error.
   * @group combinators
   */
  def encodeOnly: Codec[A @uncheckedVariance] = new Codec[A] {
    def sizeBound = self.sizeBound
    def encode(a: A) = self.encode(a)
    def decode(bits: BitVector) = Attempt.failure(Err("decoding not supported"))
  }
}

/**
 * Provides functions for working with encoders.
 *
 * @groupname conv Conveniences
 * @groupprio conv 2
 *
 */
trait EncoderFunctions {

  /**
   * Encodes the specified values, one after the other, to a bit vector using the specified encoders.
   * @group conv
   */
  final def encodeBoth[A, B](encA: Encoder[A], encB: Encoder[B])(a: A, b: B): Attempt[BitVector] = for {
    encodedA <- encA.encode(a)
    encodedB <- encB.encode(b)
  } yield encodedA ++ encodedB

  /**
   * Encodes all elements of the specified sequence and concatenates the results, or returns the first encountered error.
   * @group conv
   */
  final def encodeSeq[A](enc: Encoder[A])(seq: collection.immutable.Seq[A]): Attempt[BitVector] = {
    val buf = new collection.mutable.ArrayBuffer[BitVector](seq.size)
    seq foreach { a =>
      enc.encode(a) match {
        case Attempt.Successful(aa) => buf += aa
        case Attempt.Failure(err) => return Attempt.failure(err.pushContext(buf.size.toString))
      }
    }
    def merge(offset: Int, size: Int): BitVector = size match {
      case 0 => BitVector.empty
      case 1 => buf(offset)
      case n =>
        val half = size / 2
        merge(offset, half) ++ merge(offset + half, half + (if (size % 2 == 0) 0 else 1))
    }
    Attempt.successful(merge(0, buf.size))
  }

  /**
   * Creates an encoder that encodes with each of the specified encoders, returning
   * the first successful result.
   * @group conv
   */
  final def choiceEncoder[A](encoders: Encoder[A]*): Encoder[A] = new Encoder[A] {
    def sizeBound = SizeBound.choice(encoders.map { _.sizeBound })
    def encode(a: A) = {
      @annotation.tailrec def go(rem: List[Encoder[A]], errs: List[Err]): Attempt[BitVector] = rem match {
        case Nil => Attempt.failure(Err(errs.reverse))
        case hd :: tl =>
          hd.encode(a) match {
            case res @ Attempt.Successful(_) => res
            case Attempt.Failure(err) => go(tl, err :: errs)
          }
      }
      if (encoders.isEmpty) Attempt.failure(Err("no encoders provided"))
      else go(encoders.toList, Nil)
    }
  }
}

/**
 * Companion for [[Encoder]].
 *
 * @groupname ctor Constructors
 * @groupprio ctor 1
 *
 * @groupname inst Typeclass Instances
 * @groupprio inst 3
 */
object Encoder extends EncoderFunctions {

  /**
   * Provides syntax for summoning an `Encoder[A]` from implicit scope.
   * @group ctor
   */
  def apply[A](implicit enc: Lazy[Encoder[A]]): Encoder[A] = enc.value

  /**
   * Creates an encoder from the specified function.
   * @group ctor
   */
  def apply[A](f: A => Attempt[BitVector]): Encoder[A] = new Encoder[A] {
    def sizeBound = SizeBound.unknown
    def encode(value: A) = f(value)
  }

  /**
   * Encodes the specified value to a bit vector using an implicitly available encoder.
   * @group conv
   */
  def encode[A](a: A)(implicit e: Lazy[Encoder[A]]): Attempt[BitVector] = e.value.encode(a)

  /**
   * Transform typeclass instance.
   * @group inst
   */
  implicit val transformInstance: Transform[Encoder] = new Transform[Encoder] {
    def exmap[A, B](encoder: Encoder[A], f: A => Attempt[B], g: B => Attempt[A]): Encoder[B] =
      encoder.econtramap(g)

    override def xmap[A, B](encoder: Encoder[A], f: A => B, g: B => A): Encoder[B] =
      encoder.contramap(g)
  }
}
