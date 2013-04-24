package scodec

import scalaz.{\/, Monoid, StateT}


trait Codec[A] {
  def encode(a: A): Error \/ BitVector
  def decode(bits: BitVector): Error \/ (BitVector, A)

  def xmap[B](f: A => B, g: B => A): Codec[B] = Codec.xmap(this)(f, g)
  def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = Codec.flatZip(this)(f)
}

object Codec {

  type DecodingContext[+A] = StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A]

  object DecodingContext {

    def apply[A](f: BitVector => Error \/ (BitVector, A)): DecodingContext[A] =
      StateT[({type λ[+a] = Error \/ a})#λ, BitVector, A](f)

  }

  def decode[A](codec: Codec[A], buffer: BitVector): Error \/ A = {
    codec decode buffer map { case (rest, result) => result }
  }

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
