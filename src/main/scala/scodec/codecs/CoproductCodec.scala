package scodec
package codecs

import scalaz.\/
import scalaz.syntax.std.option._

import shapeless._
import ops.coproduct._
import ops.hlist._
import poly._

import scodec.bits._

/** Codec that encodes/decodes a coproduct `C`. */
private[scodec] class CoproductCodec[C <: Coproduct, L <: HList, A](
  codecs: L,
  discriminatorCodec: Codec[A],
  coproductToDiscriminator: C => A,
  discriminatorToIndex: A => Option[Int]
)(implicit enc: CoproductEncode[C, L], dec: ToCoproductDecoders[C, L]) extends Codec[C] {

  import CoproductCodec._

  private val decoders: List[Decoder[C]] = codecs.asDecoderList

  def encode(c: C) = {
    val discriminator = coproductToDiscriminator(c)
    for {
      encDiscriminator <- discriminatorCodec.encode(discriminator)
      encValue <- enc(c, codecs)
    } yield encDiscriminator ++ encValue
  }

  def decode(buffer: BitVector) = (for {
    discriminator <- DecodingContext(discriminatorCodec.decode)
    index <- DecodingContext.liftE(discriminatorToIndex(discriminator).toRightDisjunction(s"Unsupported discriminator $discriminator"))
    decoder <- DecodingContext.liftE(decoders.lift(index).toRightDisjunction(s"Unsupported index $index (for discriminator $discriminator)"))
    value <- DecodingContext(decoder.decode)
  } yield value).run(buffer)

  override def toString = codecs.toList.mkString("(", " :+: ", ")") + s" by $discriminatorCodec"
}

private[scodec] object CoproductCodec {

  /** Calcuates the type index of coproduct value `c` in coproduct type `C`. */
  private[scodec] def indexOf[C <: Coproduct](c: C): Int = {
    def go[CC <: Coproduct](c: CC, idx: Int): Int = c match {
      case Inl(_) => idx
      case Inr(t) => go(t, idx + 1)
    }
    go(c, 0)
  }

  implicit class AsDecoderList[L <: HList](l: L) {
    def asDecoderList[C <: Coproduct](implicit aux: ToCoproductDecoders[C, L]): List[Decoder[C]] = aux(l)
  }

  /** Creates a coproduct codec that uses the type index of the coproduct as the discriminator. */
  def indexBased[C <: Coproduct, L <: HList](codecs: L, discriminatorCodec: Codec[Int])(
    implicit enc: CoproductEncode[C, L], dec: ToCoproductDecoders[C, L]
  ): CoproductCodec[C, L, Int] = {
    new CoproductCodec(codecs, discriminatorCodec, indexOf, Some.apply)
  }
}

/** Witness that allows encoding a coproduct `C` in to a bit vector, using the codecs in `L`, which are type aligned with `C`. */
sealed trait CoproductEncode[C <: Coproduct, L <: HList] {
  def apply(c: C, codecs: L): String \/ BitVector
}

/** Companion for [[CoproductEncode]]. */
object CoproductEncode {

  implicit val base: CoproductEncode[CNil, HNil] = new CoproductEncode[CNil, HNil] {
    def apply(c: CNil, codecs: HNil) = \/.right(BitVector.empty)
  }

  implicit def step[H, CT <: Coproduct, LT <: HList](implicit encodeTail: CoproductEncode[CT, LT]): CoproductEncode[H :+: CT, Codec[H] :: LT] =
    new CoproductEncode[H :+: CT, Codec[H] :: LT] {
      def apply(c: H :+: CT, codecs: Codec[H] :: LT) = c match {
        case Inl(h) => codecs.head.encode(h)
        case Inr(t) => encodeTail(t, codecs.tail)
      }
    }
}

/** Witness that allows converting an `HList` of codecs in to a list of coproduct decoders, where the coproduct is type aligned with the `HList`. */
sealed trait ToCoproductDecoders[C <: Coproduct, L <: HList] {
  def apply(l: L): List[Decoder[C]]
}

/** Companion for [[ToCoproductDecoders]]. */
object ToCoproductDecoders {
  implicit val base: ToCoproductDecoders[CNil, HNil] = new ToCoproductDecoders[CNil, HNil] {
    def apply(hnil: HNil) = Nil
  }

  implicit def step[CH, CT <: Coproduct, A, LT <: HList](
    implicit tailAux: ToCoproductDecoders[CT, LT],
    inj: ops.coproduct.Inject[CH :+: CT, A]
  ): ToCoproductDecoders[CH :+: CT, Codec[A] :: LT] = new ToCoproductDecoders[CH :+: CT, Codec[A] :: LT] {
    def apply(l: Codec[A] :: LT): List[Decoder[CH :+: CT]] = {
      val headDecoder: Decoder[CH :+: CT] = l.head.map { a => Coproduct[CH :+: CT](a) }
      val tailDecoders: List[Decoder[CH :+: CT]] = tailAux(l.tail).map { d: Decoder[CT] =>
        d.map { ct: CT => Inr(ct): CH :+: CT }
      }
      headDecoder :: tailDecoders
    }
  }
}

/**
 * Supports building a coproduct codec.
 *
 * A coproduct codec is built by:
 *  - specifying a codec for each member of the coproduct, separated by the `:+:` operator
 *  - specifying the discriminator codec and mapping between discriminator values and coproduct members
 *
 * To specify the discriminator, call either `discriminatedByIndex(intCodec)` or `discriminatedBy(codec).using(Sized(...))`.
 * The former uses the type index as the discriminator value.
 *
 * For example: {{{
(int32 :+: bool(8) :+: variableSizeBytes(uint8, ascii)).discriminatedByIndex(uint8)
 }}}
 * The first 8 bits of the resulting binary contains the discriminator value due to usage of the `uint8` codec as
 * the discriminator codec. A discriminator value of 0 causes the remaining bits to be encoded/decoded with `int32`.
 * Similarly, a value of 1 causes the remaining bits to be encoded/decoded with `bool(8)` and a value of 2 causes
 * the remaining bits to be encoded/decoded as a sized ASCII string.
 *
 * Alternatively, discriminator values can be explicitly specified using `discriminatedBy(codec).using(Sized(...))`.
 *
 * For example: {{{
 (int32 :+: bool(8) :+: variableSizeBytes(uint8, ascii)).discriminatedBy(fixedSizeBytes(1, ascii)).using(Sized("i", "b", "s"))
 }}}
 * In this example, integers are associated with the discriminator `i`, booleans with `b`, and strings with `s`. The discriminator
 * is encoded with `fixedSizeBytes(1, ascii)`.
 */
final class CoproductCodecBuilder[C <: Coproduct, L <: HList] private[scodec] (
  codecs: L
)(implicit enc: CoproductEncode[C, L], dec: ToCoproductDecoders[C, L]) {

  /** Adds a codec to the head of this coproduct codec. */
  def :+:[A](left: Codec[A]): CoproductCodecBuilder[A :+: C, Codec[A] :: L] =
    new CoproductCodecBuilder(left :: codecs)

  /**
   * Creates the coproduct codec using the specified integer codec as the discriminator codec
   * and using coproduct indices as discriminators.
   *
   * For example, `(a :+: b :+: c).discriminatedByIndex(uint8)` results in using `0` for `a`,
   * `1` for `b`, and `2` for `c`.
   */
  def discriminatedByIndex(discriminatorCodec: Codec[Int]): Codec[C] =
    CoproductCodec.indexBased(codecs, discriminatorCodec)

  /**
   * Supports creation of a coproduct codec that uses an arbitrary discriminator.
   *
   * After calling this method, the caller must call `using(Sized(...))`, specifying the
   * discriminator value for each coproduct type, in the order that the types appear in the coproduct.
   */
  def discriminatedBy[A](discriminatorCodec: Codec[A]): NeedDiscriminators[A] =
    new NeedDiscriminators(discriminatorCodec)

  final class NeedDiscriminators[A] private[CoproductCodecBuilder] (discriminatorCodec: Codec[A]) {

    /** Specified the discriminator values for each of the coproduct type members. */
    def using[N <: Nat](discriminators: Sized[Seq[A], N])(implicit ev: ops.hlist.Length.Aux[L, N]): Codec[C] = {
      val toDiscriminator: C => A = c => discriminators.seq(CoproductCodec.indexOf(c))
      val fromDiscriminator: A => Option[Int] = a => {
        val idx = discriminators.seq.indexWhere { (x: A) => x == a }
        if (idx >= 0) Some(idx) else None
      }
      new CoproductCodec(codecs, discriminatorCodec, toDiscriminator, fromDiscriminator)
    }
  }
}
