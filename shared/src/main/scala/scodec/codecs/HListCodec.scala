package scodec
package codecs

import shapeless._
import ops.hlist.{Prepend, RightFolder, Init, Last, Length, Split}
import UnaryTCConstraint._

import scodec.bits.BitVector

private[scodec] object HListCodec {

  val hnilCodec: Codec[HNil] = new Codec[HNil] {
    override def sizeBound = SizeBound.exact(0)
    override def encode(hn: HNil) = Attempt.successful(BitVector.empty)
    override def decode(buffer: BitVector) = Attempt.successful(DecodeResult(HNil, buffer))
    override def toString = s"HNil"
  }

  def prepend[A, L <: HList](a: Codec[A], l: Codec[L]): Codec[A :: L] = new Codec[A :: L] {
    override def sizeBound = a.sizeBound + l.sizeBound
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
    override def sizeBound = l.sizeBound + a.sizeBound
    override def encode(xs: LA) = Codec.encodeBoth(l, a)(xs.init, xs.last)
    override def decode(buffer: BitVector) = Codec.decodeBothCombine(l, a)(buffer) { _ :+ _ }
    override def toString = s"append($l, $a)"
  }

  // TODO: In 1.11+, remove lengthK
  def concat[K <: HList, L <: HList, KL <: HList, KLen <: Nat](ck: Codec[K], cl: Codec[L])(implicit
    prepend: Prepend.Aux[K, L, KL],
    lengthK: Length.Aux[K, KLen],
    split: Split.Aux[KL, KLen, K, L]
  ): Codec[KL] = new Codec[KL] {
    override def sizeBound = ck.sizeBound + cl.sizeBound
    override def encode(xs: KL) = {
      val (k, l) = xs.split[KLen]
      Codec.encodeBoth(ck, cl)(k, l)
    }
    override def decode(buffer: BitVector) = Codec.decodeBothCombine(ck, cl)(buffer) { _ ::: _ }
    override def toString = s"concat($ck, $cl)"
  }

  def flatPrepend[A, L <: HList](codecA: Codec[A], f: A => Codec[L]): Codec[A :: L] = new Codec[A :: L] {
    override def sizeBound = codecA.sizeBound.atLeast
    override def encode(xs: A :: L) = Codec.encodeBoth(codecA, f(xs.head))(xs.head, xs.tail)
    override def decode(buffer: BitVector) = (for {
      a <- codecA
      l <- f(a)
    } yield a :: l).decode(buffer)
    override def toString = s"flatPrepend($codecA, $f)"
  }

  // TODO: In 1.11+, remove lengthK
  def flatConcat[K <: HList, L <: HList, KL <: HList, KLen <: Nat](codecK: Codec[K], f: K => Codec[L])(implicit
    prepend: Prepend.Aux[K, L, KL],
    lengthK: Length.Aux[K, KLen],
    split: Split.Aux[KL, KLen, K, L]
  ): Codec[KL] = new Codec[KL] {
    override def sizeBound = codecK.sizeBound.atLeast
    override def encode(xs: KL) = {
      val (k, l) = xs.split[KLen]
      Codec.encodeBoth(codecK, f(k))(k, l)
    }
    override def decode(buffer: BitVector) = (for {
      k <- codecK
      l <- f(k)
    } yield k ::: l).decode(buffer)
    override def toString = s"flatConcat($codecK, $f)"
  }

  // TODO: In 1.11+, remove length
  def flatAppend[L <: HList, A, LA <: HList, Len <: Nat](codecL: Codec[L], f: L => Codec[A])(implicit
    prepend: Prepend.Aux[L, A :: HNil, LA],
    length: Length.Aux[L, Len],
    split: Split.Aux[LA, Len, L, A :: HNil]
  ): Codec[LA] = new Codec[LA] {
    override def sizeBound = codecL.sizeBound.atLeast
    override def encode(xs: LA) = {
      val (l, rest) = xs.split[Len]
      Codec.encodeBoth(codecL, f(l))(l, rest.head)
    }
    override def decode(buffer: BitVector) = (for {
      l <- codecL
      a <- f(l)
    } yield l :+ a).decode(buffer)
    override def toString = s"flatConcat($codecL, $f)"
  }

  def apply[L <: HList : *->*[Codec]#λ, M <: HList](l: L)(implicit folder: RightFolder.Aux[L, Codec[HNil], PrependCodec.type, Codec[M]]): Codec[M] = {
    l.foldRight(hnilCodec)(PrependCodec)
  }

  def dropUnits[K <: HList, L <: HList](codec: Codec[K])(implicit du: DropUnits.Aux[K, L]) =
    codec.xmap[L](du.removeUnits, du.addUnits)
}

/**
 * Converts an `HList` of codecs in to a single codec.
 * That is, converts `Codec[X0] :: Codec[X1] :: ... :: Codec[Xn] :: HNil` in to a `Codec[X0 :: X1 :: ... :: Xn :: HNil].
 */
trait ToHListCodec[In <: HList] extends DepFn1[In] {
  type L <: HList
  type Out = Codec[L]
}

/** Companion for [[ToHListCodec]]. */
object ToHListCodec {
  type Aux[In0 <: HList, L0 <: HList] = ToHListCodec[In0] { type L = L0 }

  implicit def mk[I <: HList, L0 <: HList](implicit
    allCodecs: *->*[Codec]#λ[I],
    folder: RightFolder.Aux[I, Codec[HNil], HListCodec.PrependCodec.type, Codec[L0]]
  ): ToHListCodec.Aux[I, L0] = new ToHListCodec[I] {
    type L = L0
    def apply(i: I): Codec[L0] = HListCodec(i)
  }
}
