package scodec

import scala.language.implicitConversions

import scalaz.{ \/, InvariantFunctor, Monoid, StateT }
import \/.left
import shapeless._
import shapeless.record._
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.syntax._

import scodec.bits.BitVector

/**
 * Supports encoding a value of type `A` to a `BitVector` and decoding a `BitVector` to a value of `A`.
 *
 * Not every value of `A` can be encoded to a bit vector and similarly, not every bit vector can be decoded to a value
 * of type `A`. Hence, both encode and decode return either an error or the result. Furthermore, decode returns the
 * remaining bits in the bit vector that it did not use in decoding.
 *
 * Note: the decode function can be lifted to a state action via `StateT[Err \/ ?, BitVector, A]`. This type alias
 * and associated constructor is provided by `[[DecodingContext]]`.
 *
 *
 * @groupname tuple Tuple Support
 * @groupprio tuple 11

 * @groupname hlist HList Support
 * @groupprio hlist 12
 */
trait Codec[A] extends GenCodec[A, A] { self =>

  /**
   * Maps to a codec of type `B` using two total functions, `A => B` and `B => A`.
   * @group combinators
   */
  final def xmap[B](f: A => B, g: B => A): Codec[B] = new Codec[B] {
    def encode(b: B): Err \/ BitVector = self.encode(g(b))
    def decode(buffer: BitVector): Err \/ (BitVector, B) = self.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  /**
   * Maps to a `codec` of type `B`, where there is a partial function
   * from `B` to `A`. The encoding will fail for any `B` that
   * `g` maps to `None`.
   * @group combinators
   */
  final def pxmap[B](f: A => B, g: B => Option[A]): Codec[B] = new Codec[B] {
    def encode(b: B): Err \/ BitVector = g(b).map(self.encode).getOrElse(left(Err(s"extraction failure: $b")))
    def decode(buffer: BitVector): Err \/ (BitVector, B) = self.decode(buffer).map { case (rest, a) => (rest, f(a)) }
  }

  /**
   * Maps to a codec of type `B` using two total functions,
   * `A => Err \/ B` and `B => Err \/ A`.
   *
   * f and g can then reject their argument and return an error.
   *
   * @group combinators
   */
  final def exmap[B](f: A => Err \/ B, g: B => Err \/ A): Codec[B] = new Codec[B] {
     def encode(b: B): Err \/ BitVector = g(b) flatMap self.encode
     def decode(buffer: BitVector): Err \/ (BitVector, B) =
       self.decode(buffer) flatMap { case (rest, a) => f(a).flatMap { b => \/.right((rest, b)) } }
  }

  /**
   * Maps to a codec of type `B` using a total function on input to encode and a partial function on output from decode.
   *
   * The supplied functions form an injection from `B` to `A`. Hence, converting a `Codec[A]` to a `Codec[B]` converts from
   * a larger to a smaller type. Hence, the name `narrow`.
   *
   * @group combinators
   */
  final def narrow[B](f: A => Err \/ B, g: B => A): Codec[B] =
    exmap(f, \/.right compose g)


  /**
   * Maps to a codec of type `B` using a partial function on input to encode and a total function on output from decode.
   *
   * The supplied functions form an injection from `A` to `B`. Hence, converting a `Codec[A]` to a `Codec[B]` converts from
   * a smaller to a larger type. Hence, the name `widen`.

   * @group combinators
   */
  final def widen[B](f: A => B, g: B => Err \/ A): Codec[B] =
    exmap(\/.right compose f, g)

  /**
   * Returns a new codec that encodes/decodes a value of type `B` by using an isomorphism between `A` and `B`.
   *
   * The isomorphism is provided by the implicit `CodecAsAux` instance.
   *
   * Typically used when `B` is a case class and `A` is an `HList` with the same shape as the elements of `B`.
   *
   * @group hlist
   */
  final def as[B](implicit as: CodecAsAux[B, A]): Codec[B] = as(this)

  /**
   * Lifts this codec in to a codec of a singleton hlist.
   *
   * @group hlist
   */
  final def hlist: Codec[A :: HNil] = xmap(_ :: HNil, _.head)

  /**
   * Creates a `Codec[(A, B)]` that first encodes/decodes an `A` followed by a `B`.
   * @group tuple
   */
  final def pairedWith[B](codecB: Codec[B]): Codec[(A, B)] = new codecs.TupleCodec(this, codecB)

  /**
   * Creates a `Codec[(A, B)]` that first encodes/decodes an `A` followed by a `B`.
   *
   * Operator alias for [[pairedWith]].
   * @group tuple
   */
  final def ~[B](codecB: Codec[B]): Codec[(A, B)] = pairedWith(codecB)

  /**
   * Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
   * decodes a unit followed by a `B` and discards the decoded unit.
   *
   * @group tuple
   */
  final def dropLeft[B](codecB: Codec[B])(implicit ev: Unit =:= A): Codec[B] =
    pairedWith(codecB).xmap[B]({ case (a, b) => b }, b => (ev(()), b))

  /**
   * Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
   * decodes a unit followed by a `B` and discards the decoded unit.
   *
   * Operator alias of [[dropLeft]].
   * @group tuple
   */
  final def ~>[B](codecB: Codec[B])(implicit ev: Unit =:= A): Codec[B] = dropLeft(codecB)

  /**
   * Assuming `B` is `Unit`, creates a `Codec[A]` that: encodes the `A` followed by a unit;
   * decodes an `A` followed by a unit and discards the decoded unit.
   *
   * @group tuple
   */
  final def dropRight[B](codecB: Codec[B])(implicit ev: Unit =:= B): Codec[A] =
    pairedWith(codecB).xmap[A]({ case (a, b) => a }, a => (a, ev(())))

  /**
   * Assuming `B` is `Unit`, creates a `Codec[A]` that: encodes the `A` followed by a unit;
   * decodes an `A` followed by a unit and discards the decoded unit.
   *
   * Operator alias of [[dropRight]].
   * @group tuple
   */
  final def <~[B](codecB: Codec[B])(implicit ev: Unit =:= B): Codec[A] = dropRight(codecB)

  /**
   * Converts this to a `Codec[Unit]` that encodes using the specified zero value and
   * decodes a unit value when this codec decodes an `A` successfully.
   *
   * @group combinators
   */
  final def unit(zero: A): Codec[Unit] = xmap[Unit](_ => (), _ => zero)

  /**
   * Converts this to a `Codec[Unit]` that encodes using the zero value of the implicitly
   * available `Monoid[A]` and decodes a unit value when this codec decodes an `A` successfully.
   *
   * @group combinators
   */
  final def unitM(implicit ma: Monoid[A]): Codec[Unit] = unit(ma.zero)

  /**
   * Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
   * @group tuple
   */
  final def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
    override def encode(t: (A, B)) = Codec.encodeBoth(self, f(t._1))(t._1, t._2)
    override def decode(buffer: BitVector) = (for {
      a <- DecodingContext(self.decode)
      b <- DecodingContext(f(a).decode)
    } yield (a, b)).run(buffer)
  }

  /**
   * Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
   * Operator alias for [[flatZip]].
   * @group tuple
   */
  final def >>~[B](f: A => Codec[B]): Codec[(A, B)] = flatZip(f)

  final override def complete: Codec[A] = Codec(this, super.complete)

  final override def compact: Codec[A] = Codec(super.compact, this)

  /**
   * Creates a new codec that is functionally equivalent to this codec but returns the specified string from `toString`.
   * @group combinators
   */
  final def withToString(str: String): Codec[A] = new Codec[A] {
    override def encode(a: A) = self.encode(a)
    override def decode(buffer: BitVector) = self.decode(buffer)
    override def toString = str
  }

  /**
   * Supports creation of a coproduct codec. See [[scodec.codecs.CoproductCodecBuilder]] for details.
   * @group coproduct
   */
  def :+:[B](left: Codec[B]): codecs.CoproductCodecBuilder[B :+: A :+: CNil, Codec[B] :: Codec[A] :: HNil] =
    new codecs.CoproductCodecBuilder(left :: self :: HNil)

  /**
   * Lifts this codec to a codec of a shapeless field -- allowing it to be used in records and unions.
   * @group combinators
   */
  def toField[K]: Codec[FieldType[K, A]] =
    xmap[FieldType[K, A]](a => field[K](a), identity)
}

/**
 * Typeclass that witnesses that a `Codec[A]` can be xmapped in to a `Codec[B]`.
 *
 * Implicit instances (forward and reverse) are provided between:
 *  - case classes and `HList`s of compatible shapes
 *  - singleton case classes and values of proper types
 *
 * Credit: Miles Sabin
 */
@annotation.implicitNotFound("""Could not prove that ${B} can be converted to/from ${A}.
Proof is automatically available between case classes and HLists that have the same shape, as singleton case classes and values of matching types.""")
abstract class CodecAsAux[B, A] {
  def apply(ca: Codec[A]): Codec[B]
}

/** Companion for [[CodecAsAux]]. */
object CodecAsAux {

  /** Provides a `CodecAsAux[B, A]` for case class `B` and HList `A`. */
  implicit def mkAs[B, Repr, A](implicit gen: Generic.Aux[B, Repr], aToR: A =:= Repr, rToA: Repr =:= A): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.from(a)
      val to: B => A = b => gen.to(b)
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAsAux[B, A]` for HList `B` and case class `A`. */
  implicit def mkAsReverse[B, Repr, A](implicit gen: Generic.Aux[A, Repr], bToR: B =:= Repr, rToB: Repr =:= B): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.to(a)
      val to: B => A = b => gen.from(b)
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAsAux[B, A]` for singleton case class `B` and value `A`. */
  implicit def mkAsSingleton[B, Repr, A](implicit gen: Generic.Aux[B, Repr], aToR: (A :: HNil) =:= Repr, rToA: Repr =:= (A :: HNil)): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.from(a :: HNil)
      val to: B => A = b => gen.to(b).head
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAsAux[B, A]` for value `B` and singleton case class `A`. */
  implicit def mkAsSingletonReverse[B, Repr, A](implicit gen: Generic.Aux[A, Repr], bToR: (B :: HNil) =:= Repr, rToB: Repr =:= (B :: HNil)): CodecAsAux[B, A]  = new CodecAsAux[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.to(a).head
      val to: B => A = b => gen.from(b :: HNil)
      ca.xmap(from, to)
    }
  }
}

/** Companion for [[Codec]]. */
object Codec extends EncoderFunctions with DecoderFunctions {

  /** Creates a codec from encoder and decoder functions. */
  def apply[A](encoder: A => Err \/ BitVector, decoder: BitVector => Err \/ (BitVector, A)): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder(a)
    override def decode(bits: BitVector) = decoder(bits)
  }

  /** Creates a codec from an encoder and a decoder. */
  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder.encode(a)
    override def decode(bits: BitVector) = decoder.decode(bits)
  }

  /** Gets the implicitly available codec for type `A`. */
  def apply[A: Codec]: Codec[A] = implicitly[Codec[A]]

  /**
   * Automatically creates an hlist based codec for the specified type.
   *
   * Support exists for `HList`s, records, and case classes.
   * Each component type must have an implicitly available codec.
   *
   * For example:
   {{{
  case class Point(x: Int, y: Int, z: Int)
  implicit val ic = uint8
  val res = Codec.auto[Point].encode(Point(1, 2, 3))
  res: scalaz.\/[scodec.Err,scodec.bits.BitVector] = \/-(BitVector(24 bits, 0x010203))
   }}}
   *
   * For records and case classes, the record name / field name is included on
   * each component codec. For example:
   {{{
  val res = Codec.auto[Point].encode(Point(1, 2, 256))
  res: scalaz.\/[scodec.Err,scodec.bits.BitVector] = -\/(z: 256 is greater than maximum value 255 for 8-bit unsigned integer)
   }}}
   */
  def auto[A](implicit auto: Auto[A]): Codec[A] = auto.codec

  /** Witness that a codec of type `A` can be automatically created. */
  sealed trait Auto[A] {
    def codec: Codec[A]
  }

  /** Companion for [[Auto]]. */
  object Auto {
    implicit def hnil: Auto[HNil] =
      new Auto[HNil] { val codec = codecs.HListCodec.hnilCodec }

    implicit def hlist[H, T <: HList](implicit headCodec: Codec[H], tailAux: Auto[T]): Auto[H :: T] =
      new Auto[H :: T] { val codec = headCodec :: tailAux.codec }

    implicit def record[KH <: Symbol, VH, TRec <: HList, KT <: HList](implicit
      headCodec: Codec[VH],
      tailAux: Auto[TRec],
      keys: Keys.Aux[FieldType[KH, VH] :: TRec, KH :: KT]
    ): Auto[FieldType[KH, VH] :: TRec] = new Auto[FieldType[KH, VH] :: TRec] {
      val codec = {
        import codecs.StringEnrichedWithCodecNamingSupport
        val namedHeadCodec: Codec[VH] = keys().head.name | headCodec
        val headFieldCodec: Codec[FieldType[KH, VH]] = namedHeadCodec.toField[KH]
        headFieldCodec :: tailAux.codec
      }
    }

    implicit def labelledProduct[A, Rec <: HList](implicit
      lgen: LabelledGeneric.Aux[A, Rec],
      auto: Auto[Rec]
    ): Auto[A] = new Auto[A] {
      val codec = auto.codec.xmap(lgen.from, lgen.to)
    }
  }

  /**
   * Creates a coproduct codec builder for the specified type.
   *
   * Support exists for coproducts and unions.
   * Each component type must have an implicitly available codec.
   *
   * For example:
   {{{
  type C = Foo :+: Bar :+: Baz :+: CNil
  val codec = Codec.coproduct[C].choice
  codec.encode(Coproduct[C](Foo(...)))
   }}}
   */
  def coproduct[A](implicit auto: CoproductAuto[A]): auto.Out = auto.apply

  /** Witness that a coproduct codec builder of type `A` can be automatically created. */
  sealed trait CoproductAuto[A] extends DepFn0 {
    type C <: Coproduct
    type L <: HList
    type Out = codecs.CoproductCodecBuilder[C, L]
    def apply: Out
  }

  /** Companion for [[CoproductAuto]]. */
  object CoproductAuto {
    type Aux[A, C0] = CoproductAuto[A] { type C = C0 }

    implicit def cnil: CoproductAuto.Aux[CNil, CNil] =
      new CoproductAuto[CNil] {
        type C = CNil
        type L = HNil
        def apply = new codecs.CoproductCodecBuilder[CNil, HNil](HNil)
      }

    implicit def coproduct[H, T <: Coproduct](implicit
      headCodec: Codec[H],
      tailAux: CoproductAuto.Aux[T, T]
    ): CoproductAuto.Aux[H :+: T, H :+: T] =
      new CoproductAuto[H :+: T] {
        type C = H :+: T
        type L = Codec[H] :: tailAux.L
        def apply = headCodec :+: tailAux.apply
      }

    import shapeless.ops.union.{ Keys => UnionKeys }

    implicit def union[KH <: Symbol, VH, T <: Coproduct, KT <: HList](implicit
      headCodec: Codec[VH],
      tailAux: CoproductAuto.Aux[T, T],
      keys: UnionKeys.Aux[FieldType[KH, VH] :+: T, KH :: KT]
    ): CoproductAuto.Aux[FieldType[KH, VH] :+: T, FieldType[KH, VH] :+: T] =
      new CoproductAuto[FieldType[KH, VH] :+: T] {
        type C = FieldType[KH, VH] :+: T
        type L = Codec[FieldType[KH, VH]] :: tailAux.L
        def apply = {
          import codecs.StringEnrichedWithCodecNamingSupport
          val namedHeadCodec: Codec[VH] = keys().head.name | headCodec
          namedHeadCodec.toField[KH] :+: tailAux.apply
        }
      }
  }

  val invariantFunctorInstance: InvariantFunctor[Codec] = new InvariantFunctor[Codec] {
    def xmap[A, B](c: Codec[A], f: A => B, g: B => A) = c.xmap(f, g)
  }
}
