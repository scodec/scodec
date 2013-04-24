package scodec

import scalaz.\/-

import shapeless._
import TypeOperators._
import UnaryTCConstraint._


object HListCodec {

  val emptyHListCodec: Codec[HNil] = new Codec[HNil] {
    def encode(hn: HNil) = \/-(BitVector.empty)
    def decode(buffer: BitVector) = \/-((BitVector.empty, HNil))
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

  def apply[L <: HList : *->*[Codec]#λ, M <: HList](l: L)(implicit folder: RightFolder[L, Codec[HNil], PrependCodec.type]) = {
    l.foldRight(emptyHListCodec)(PrependCodec)
  }
}

trait HListCodecSyntax {
  import HListCodec._

  /** Provides common operations on a `Codec[HList]`. */
  implicit class EnrichedHListCodec[L <: HList](l: Codec[L]) {

    /** Returns a new codec representing `Codec[A :: L]`. */
    def :~:[A](a: Codec[A]): Codec[A :: L] = prependCodec(a, l)

    /** Returns a new codec that encodes/decodes `A :: L` but only returns `L`.  HList equivalent of `~>`. */
    def :~>:[A: scalaz.Monoid](a: Codec[A]): Codec[L] = Codec.dropLeft(a, l)

    /** Returns a new codec that encodes/decodes a value of type `A` by using an iso between `A` and `L`. */
    def as[A](implicit iso: Iso[A, L]): Codec[A] = l.xmap(iso.from _, iso.to _)
  }

  /** Provides `HList` related syntax for codecs of any type. */
  implicit class EnrichedCodec[A](a: Codec[A]) {

    /** Creates a new codec that encodes/decodes an `HList` of `B :: A :: HNil`. */
    def :~:[B](b: Codec[B]): Codec[B :: A :: HNil] = prependCodec(b, prependCodec(a, emptyHListCodec))
  }
}
