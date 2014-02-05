package scodec

import scalaz.\/-

import shapeless._
import UnaryTCConstraint._


object HListCodec {

  val hnilCodec: Codec[HNil] = new Codec[HNil] {
    override def encode(hn: HNil) = \/-(BitVector.empty)
    override def decode(buffer: BitVector) = \/-((buffer, HNil))
    override def toString = s"HNil"
  }

  def prepend[A, L <: HList](a: Codec[A], l: Codec[L]): Codec[A :: L] = new Codec[A :: L] {
    override def encode(xs: A :: L) = Codec.encodeBoth(a, l)(xs.head, xs.tail)
    override def decode(buffer: BitVector) = (for {
      decA <- DecodingContext(a.decode)
      decL <- DecodingContext(l.decode)
    } yield decA :: decL).run(buffer)
    override def toString = s"$a :: $l"
  }

  object Prepend extends Poly2 {
    implicit def caseCodecAndCodecHList[A, L <: HList] = at[Codec[A], Codec[L]](prepend)
  }

  def append[L <: HList, A, LA <: HList](l: Codec[L], a: Codec[A])(implicit
    prepend: PrependAux[L, A :: HNil, LA],
    init: Init[LA] { type Out = L },
    last: Last[LA] { type Out = A }
  ): Codec[LA] = new Codec[LA] {
    override def encode(xs: LA) = Codec.encodeBoth(l, a)(xs.init, xs.last)
    override def decode(buffer: BitVector) = (for {
      decL <- DecodingContext(l.decode)
      decA <- DecodingContext(a.decode)
    } yield decL :+ decA).run(buffer)
    override def toString = s"append($l, $a)"
  }

  def concat[K <: HList, L <: HList, KL <: HList, KLen <: Nat](ck: Codec[K], cl: Codec[L])(implicit
    prepend: PrependAux[K, L, KL],
    lengthK: Length[K] { type Out = KLen },
    split: Split[KL, KLen] { type P = K; type S = L }
  ): Codec[KL] = new Codec[KL] {
    override def encode(xs: KL) = {
      val (k, l) = xs.split[KLen]
      Codec.encodeBoth(ck, cl)(k, l)
    }
    override def decode(buffer: BitVector) = (for {
      decK <- DecodingContext(ck.decode)
      decL <- DecodingContext(cl.decode)
    } yield decK ::: decL).run(buffer)
    override def toString = s"concat($ck, $cl)"
  }

  def apply[L <: HList : *->*[Codec]#λ, M <: HList](l: L)(implicit folder: RightFolderAux[L, Codec[HNil], Prepend.type, Codec[M]]): Codec[M] = {
    l.foldRight(hnilCodec)(Prepend)
  }
}

private[scodec] trait HListCodecSyntax {
  import HListCodec._

  /** Provides common operations on a `Codec[HList]`. */
  final implicit class EnrichedHListCodec[L <: HList](l: Codec[L]) {

    /** Returns a new codec representing `Codec[A :: L]`. */
    def ::[A](a: Codec[A]): Codec[A :: L] = prepend(a, l)

    /** Returns a new codec that encodes/decodes `A :: L` but only returns `L`.  HList equivalent of `~>`. */
    def :~>:[A: scalaz.Monoid](a: Codec[A]): Codec[L] = Codec.dropLeft(a, l)

    /** Returns a new codec that encodes/decodes the `HList L` followed by an `A`. */
    def :+[A, LA <: HList](a: Codec[A])(implicit
      prepend: PrependAux[L, A :: HNil, LA],
      init: Init[LA] { type Out = L },
      last: Last[LA] { type Out = A }
    ): Codec[LA] =
      append(l, a)

    /** Returns a new codec the encodes/decodes the `HList K` followed by the `HList L`. */
    def :::[K <: HList, KL <: HList, KLen <: Nat](k: Codec[K])(implicit
      prepend: PrependAux[K, L, KL],
      lengthK: Length[K] { type Out = KLen },
      split: Split[KL, KLen] { type P = K; type S = L }
    ): Codec[KL] = concat(k, l)
  }

  /** Provides `HList` related syntax for codecs of any type. */
  final implicit class EnrichedCodec[A](codecA: Codec[A]) {

    /** Creates a new codec that encodes/decodes an `HList` of `B :: A :: HNil`. */
    def ::[B](codecB: Codec[B]): Codec[B :: A :: HNil] =
      prepend(codecB, prepend(codecA, hnilCodec))

    def flatPrepend[L <: HList](f: A => Codec[L]): Codec[A :: L] = new Codec[A :: L] {
      override def encode(xs: A :: L) = Codec.encodeBoth(codecA, f(xs.head))(xs.head, xs.tail)
      override def decode(buffer: BitVector) = (for {
        a <- DecodingContext(codecA.decode)
        l <- DecodingContext(f(a).decode)
      } yield a :: l).run(buffer)
      override def toString = s"flatPrepend($codecA, $f)"
    }

    def flatZipHList[B](f: A => Codec[B]): Codec[A :: B :: HNil] = flatPrepend(f andThen (_.hlist))

    /** Operator alias for `flatPrepend`. */
    def >>:~[L <: HList](f: A => Codec[L]): Codec[A :: L] = flatPrepend(f)
  }
}
