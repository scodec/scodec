package scodec

import scalaz.\/-

import shapeless._
import TypeOperators._
import UnaryTCConstraint._


object HListCodec {

  val emptyHListCodec: Codec[HNil] = new Codec[HNil] {
    def encode(hn: HNil) = \/-(BitVector.empty)
    def decode(buffer: BitVector) = \/-((buffer, HNil))
  }

  def prependCodec[A, L <: HList](a: Codec[A], l: Codec[L]): Codec[A :: L] = new Codec[A :: L] {
    override def encode(xs: A :: L) = Codec.encodeBoth(a, l)(xs.head, xs.tail)
    override def decode(buffer: BitVector) = (for {
      decA <- Codec.DecodingContext(a.decode)
      decL <- Codec.DecodingContext(l.decode)
    } yield decA :: decL).run(buffer)
  }

  object PrependCodec extends Poly2 {
    implicit def caseCodecAndCodecHList[A, L <: HList] = at[Codec[A], Codec[L]](prependCodec)
  }

  def appendCodec[L <: HList, A, LA <: HList](l: Codec[L], a: Codec[A])(implicit
    prepend: PrependAux[L, A :: HNil, LA],
    init: Init[LA] { type Out = L },
    last: Last[LA] { type Out = A }
  ): Codec[LA] = new Codec[LA] {
    override def encode(xs: LA) = Codec.encodeBoth(l, a)(xs.init, xs.last)
    override def decode(buffer: BitVector) = (for {
      decL <- Codec.DecodingContext(l.decode)
      decA <- Codec.DecodingContext(a.decode)
    } yield decL :+ decA).run(buffer)
  }

  def concatCodecs[K <: HList, L <: HList, KL <: HList, KLen <: Nat](ck: Codec[K], cl: Codec[L])(implicit
    prepend: PrependAux[K, L, KL],
    lengthK: Length[K] { type Out = KLen },
    split: Split[KL, KLen] { type P = K; type S = L }
  ): Codec[KL] = new Codec[KL] {
    override def encode(xs: KL) = {
      val (k, l) = xs.split[KLen]
      Codec.encodeBoth(ck, cl)(k, l)
    }
    override def decode(buffer: BitVector) = (for {
      decK <- Codec.DecodingContext(ck.decode)
      decL <- Codec.DecodingContext(cl.decode)
    } yield decK ::: decL).run(buffer)
  }

  def apply[L <: HList : *->*[Codec]#λ, M <: HList](l: L)(implicit folder: RightFolder[L, Codec[HNil], PrependCodec.type]) = {
    l.foldRight(emptyHListCodec)(PrependCodec)
  }
}

trait HListCodecSyntax {
  import HListCodec._

  /** Provides common operations on a `Codec[HList]`. */
  implicit class EnrichedHListCodec[L <: HList](l: Codec[L]) {

    /** Returns a new codec representing `Codec[A :: L]`. */
    def ::[A](a: Codec[A]): Codec[A :: L] = prependCodec(a, l)

    /** Returns a new codec that encodes/decodes `A :: L` but only returns `L`.  HList equivalent of `~>`. */
    def :~>:[A: scalaz.Monoid](a: Codec[A]): Codec[L] = Codec.dropLeft(a, l)

    /** Returns a new codec that encodes/decodes the `HList L` followed by an `A`. */
    def :+[A, LA <: HList](a: Codec[A])(implicit
      prepend: PrependAux[L, A :: HNil, LA],
      init: Init[LA] { type Out = L },
      last: Last[LA] { type Out = A }
    ): Codec[LA] =
      appendCodec(l, a)

    /** Returns a new codec the encodes/decodes the `HList K` followed by the `HList L`. */
    def :::[K <: HList, KL <: HList, KLen <: Nat](k: Codec[K])(implicit
      prepend: PrependAux[K, L, KL],
      lengthK: Length[K] { type Out = KLen },
      split: Split[KL, KLen] { type P = K; type S = L }
    ): Codec[KL] = concatCodecs(k, l)
  }

  /** Provides `HList` related syntax for codecs of any type. */
  implicit class EnrichedCodec[A](codecA: Codec[A]) {

    /** Creates a new codec that encodes/decodes an `HList` of `B :: A :: HNil`. */
    def ::[B](codecB: Codec[B]): Codec[B :: A :: HNil] =
      prependCodec(codecB, prependCodec(codecA, emptyHListCodec))

    def flatPrepend[L <: HList](f: A => Codec[L]): Codec[A :: L] = new Codec[A :: L] {
      override def encode(xs: A :: L) = Codec.encodeBoth(codecA, f(xs.head))(xs.head, xs.tail)
      override def decode(buffer: BitVector) = (for {
        a <- Codec.DecodingContext(codecA.decode)
        l <- Codec.DecodingContext(f(a).decode)
      } yield a :: l).run(buffer)
    }

    def flatZipHList[B](f: A => Codec[B]): Codec[A :: B :: HNil] = flatPrepend(f andThen (_.hlist))

    /** Operator alias for `flatPrepend`. */
    def >>:~[L <: HList](f: A => Codec[L]): Codec[A :: L] = flatPrepend(f)
  }
}
