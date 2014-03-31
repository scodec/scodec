package scodec
package codecs

import scalaz.{ \/-, -\/ }

import shapeless._
import ops.hlist.{Prepend, RightFolder, Init, Last, Length, Split}
import UnaryTCConstraint._

import scodec.bits.BitVector

private[scodec] object HListCodec {

  val hnilCodec: Codec[HNil] = new Codec[HNil] {
    override def encode(hn: HNil) = \/-(BitVector.empty)
    override def decode(buffer: BitVector) = \/-((buffer, HNil))
    override def toString = s"HNil"
  }

  def prepend[A, L <: HList](a: Codec[A], l: Codec[L]): Codec[A :: L] = new Codec[A :: L] {
    override def encode(xs: A :: L) = Codec.encodeBoth(a, l)(xs.head, xs.tail)
    override def decode(buffer: BitVector) = Codec.decodeBothCombine(a, l)(buffer) { _ :: _ }
    override def toString = s"$a :: $l"
  }

  object PrependCodec extends Poly2 {
    implicit def caseCodecAndCodecHList[A, L <: HList] = at[Codec[A], Codec[L]](prepend)
  }

  def append[L <: HList, A, LA <: HList](l: Codec[L], a: Codec[A])(implicit
    prepend: Prepend.Aux[L, A :: HNil, LA],
    init: Init.Aux[LA, L],
    last: Last.Aux[LA, A]
  ): Codec[LA] = new Codec[LA] {
    override def encode(xs: LA) = Codec.encodeBoth(l, a)(xs.init, xs.last)
    override def decode(buffer: BitVector) = Codec.decodeBothCombine(l, a)(buffer) { _ :+ _ }
    override def toString = s"append($l, $a)"
  }

  def concat[K <: HList, L <: HList, KL <: HList, KLen <: Nat](ck: Codec[K], cl: Codec[L])(implicit
    prepend: Prepend.Aux[K, L, KL],
    lengthK: Length.Aux[K, KLen],
    split: Split.Aux[KL, KLen, (K, L)]
  ): Codec[KL] = new Codec[KL] {
    override def encode(xs: KL) = {
      val (k, l) = xs.split[KLen]
      Codec.encodeBoth(ck, cl)(k, l)
    }
    override def decode(buffer: BitVector) = Codec.decodeBothCombine(ck, cl)(buffer) { _ ::: _ }
    override def toString = s"concat($ck, $cl)"
  }

  def flatPrepend[A, L <: HList](codecA: Codec[A], f: A => Codec[L]): Codec[A :: L] = new Codec[A :: L] {
    override def encode(xs: A :: L) = Codec.encodeBoth(codecA, f(xs.head))(xs.head, xs.tail)
    override def decode(buffer: BitVector) = (for {
      a <- DecodingContext(codecA.decode)
      l <- DecodingContext(f(a).decode)
    } yield a :: l).run(buffer)
    override def toString = s"flatPrepend($codecA, $f)"
  }

  def apply[L <: HList : *->*[Codec]#λ, M <: HList](l: L)(implicit folder: RightFolder.Aux[L, Codec[HNil], PrependCodec.type, Codec[M]]): Codec[M] = {
    l.foldRight(hnilCodec)(PrependCodec)
  }
}

