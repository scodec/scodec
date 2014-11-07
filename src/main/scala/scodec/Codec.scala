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
 * There are various ways to create instances of `Codec`. The trait can be implemented directly or one of the
 * constructor methods in the companion can be used (e.g., `apply`, `derive`). Most of the methods on `Codec`
 * create return a new codec that has been transformed in some way. For example, the [[xmap]] method
 * converts a `Codec[A]` to a `Codec[B]` given two functions, `A => B` and `B => A`.
 *
 * One of the simplest transformation methods is `def withContext(context: String): Codec[A]`, which
 * pushes the specified context string in to any errors (i.e., `Err`s) returned from encode or decode.
 *
 * See the methods on this trait for additional transformation types.
 *
 * See the [[codecs]] package object for pre-defined codecs for many common data types and combinators for building larger
 * codecs out of smaller ones.
 *
 * == Tuple Codecs ==
 *
 * The `~` operator supports combining a `Codec[A]` and a `Codec[B]` in to a `Codec[(A, B)]`.
 *
 * For example: {{{
   val codec: Codec[Int ~ Int ~ Int] = uint8 ~ uint8 ~ uint8}}}
 *
 * Codecs generated with `~` result in left nested tuples. These left nested tuples can
 * be pulled back apart by pattern matching with `~`. For example: {{{
  Codec.decode(uint8 ~ uint8 ~ uint8, bytes) map { case a ~ b ~ c => a + b + c }
 }}}
 *
 * Alternatively, a function of N arguments can be lifted to a function of left-nested tuples. For example: {{{
  val add3 = (_: Int) + (_: Int) + (_: Int)
  Codec.decode(uint8 ~ uint8 ~ uint8, bytes) map add3
 }}}
 *
 * Similarly, a left nested tuple can be created with the `~` operator. This is useful when creating the tuple structure
 * to pass to encode. For example: {{{
  (uint8 ~ uint8 ~ uint8).encode(1 ~ 2 ~ 3)
 }}}
 *
 * Tuple based codecs are of limited use compared to `HList` based codecs, which is discussed later.
 *
 * Note: this design is heavily based on Scala's parser combinator library and the syntax it provides.
 *
 * === flatZip ===
 *
 * Sometimes when combining codecs, a latter codec depends on a formerly decoded value.
 * The `flatZip` method is important in these types of situations -- it represents a dependency between
 * the left hand side and right hand side. Its signature is `def flatZip[B](f: A => Codec[B]): Codec[(A, B)]`.
 * This is similar to `flatMap` except the return type is `Codec[(A, B)]` instead of `Decoder[B]`.
 *
 * Consider a binary format of an 8-bit unsigned integer indicating the number of bytes following it.
 * To implement this with `flatZip`, we could write: {{{
  val x: Codec[(Int, ByteVector)] = uint8 flatZip { numBytes => bytes(numBytes) }
  val y: Codec[ByteVector] = x.xmap[ByteVector]({ case (_, bv) => bv }, bv => (bv.size, bv))
 }}}
 * In this example, `x` is a `Codec[(Int, ByteVector)]` but we do not need the size directly in the model
 * because it is redundant with the size stored in the `ByteVector`. Hence, we remove the `Int` by
 * `xmap`-ping over `x`. The notion of removing redundant data from models comes up frequently.
 * Note: there is a combinator that expresses this pattern more succinctly -- `variableSizeBytes(uint8, bytes)`.
 *
 * == HList Codecs ==
 *
 * `HList`s are similar to tuples in that they represent the product of an arbitrary number of types. That is,
 * the size of an `HList` is known at compile time and the type of each element is also known at compile time.
 * For more information on `HList`s in general, see [[https://github.com/milessabin/shapeless Shapeless]].
 *
 * `Codec` makes heavy use of `HList`s. The primary operation is extending a `Codec[L]` for some `L <: HList` to
 * a `Codec[A :: L]`. For example: {{{
  val uint8: Codec[Int] = ...
  val string: Codec[String] = ...
  val codec: Codec[Int :: Int :: String] = uint8 :: uint8 :: string}}}
 * The `::` method is sort of like cons-ing on to the `HList` but it is doing so *inside* the `Codec` type.
 * The resulting codec encodes values by passing each component of the `HList` to the corresponding codec
 * and concatenating all of the results.
 *
 * There are various methods on this trait that only work on `Codec[L]` for some `L <: HList`. Besides the aforementioned
 * `::` method, there are others like `:::`, `flatPrepend`, `flatConcat`, etc. One particularly useful method is
 * `dropUnits`, which removes any `Unit` values from the `HList`.
 *
 * Given a `Codec[X0 :: X1 :: ... Xn :: HNil]` and a case class with types `X0` to `Xn` in the same order,
 * the `HList` codec can be turned in to a case class codec via the `as` method. For example:
 {{{
  case class Point(x: Int, y: Int, z: Int)
  val threeInts: Codec[Int :: Int :: Int :: HNil] = uint8 :: uint8 :: uint8
  val point: Codec[Point] = threeInts.as[Point]
 }}}
 *
 * === flatPrepend ===
 *
 * The `HList` analog to `flatZip` is `flatPrepend`. It has the signature: {{{
  def flatPrepend[L <: HList](f: A => Codec[L]): Codec[A :: L]
 }}}
 * It forms a codec of `A` consed on to `L` when called on a `Codec[A]` and passed a function `A => Codec[L]`.
 * Note that the specified function must return an `HList` based codec. Implementing our example from earlier
 * using `flatPrepend`: {{{
  val x: Codec[Int :: ByteVector :: HNil] = uint8 flatPrepend { numBytes => bytes(numBytes).hlist }
 }}}
 * In this example, `bytes(numBytes)` returns a `Codec[ByteVector]` so we called `.hlist` on it to lift it
 * in to a `Codec[ByteVector :: HNil]`.
 *
 * There are similar methods for flat appending and flat concating.
 *
 * == Coproduct Codecs ==
 *
 * Given some ordered list of types, potentially with duplicates, a value of the `HList` of those types
 * has a value for *every* type in the list. In other words, an `HList` represents having an `X0` AND `X1` AND
 * ... AND `XN`. A `Coproduct` for the same list of types represents having a value for *one* of those types.
 * In other words, a `Coproduct` represents having an `X0` OR `X1` OR ... OR `XN`. This is somewhat imprecise
 * because a coproduct can tell us exactly which `Xi` we have, even in the presence of duplicate types.
 *
 * A coproduct can also be thought of as an `Either` that has an unlimited number of choices instead of just 2 choices.
 *
 * Shapeless represents coproducts in a similar way as `HList`s. A coproduct type is built using the `:+:` operator
 * with a sentinal value of `CNil`. For example, an `Int` or `Long` or `String` is represented as the coproduct type: {{{
  Int :+: Long :+: String :+: CNil }}}
 *
 * For more information on coproducts in general, see [[https://github.com/milessabin/shapeless Shapeless]].
 *
 * Like `HList` based codecs, scodec supports `Coproduct` based codecs by coopting syntax from Shapeless. Specifically,
 * the `:+:` operator is used: {{{
  val builder = uint8 :+: int64 :+: utf8
 }}}
 * Unlike `HList` based codecs, the result of `:+:` is not a codec but rather a [[codecs.CoproductCodecBuilder]].
 * Having a list of types and a codec for each is not sufficient to build a coproduct codec. We also need to describe
 * how each entry in the coproduct is differentiated from the other entries. There are a number of ways to do this
 * and each way changes the binary format significantly. See the docs on `CoproductCodecBuilder` for details.
 *
 * == Derived Codecs ==
 *
 * Codecs for case classes and sealed class hierarchies can often be automatically derived.
 *
 * Consider this example: {{{
  import scodec.codecs.implicits._
  case class Point(x: Int, y: Int, z: Int)
  Codec[Point].encode(Point(1, 2, 3))
 }}}
 * In this example, no explicit codec was defined for `Point` yet `Codec[Point]` successfully created one.
 * It did this by "reflecting" over the structure of `Point` and looking up a codec for each component type
 * (note: no runtime reflection is performed - rather, this is implemented using macro-based compile time reflection).
 * In this case, there are three components, each of type `Int`, so it looked for an implicit `Codec[Int]`.
 * It then combined each `Codec[Int]` using an `HList` based codec and finally converted the `HList` codec
 * to a `Codec[Point]`. It found the implicit `Codec[Int]` instances due to the import of `scodec.codecs.implicits._`.
 * Furthermore, if there was an error encoding or decoding a field, the field name (i.e., x, y, or z) is included
 * as context on the `Err` returned.
 *
 * This works similarly for sealed class hierarchies -- each subtype is internally represented as a member
 * of a coproduct. There must be the following implicits in scope however:
 *  - `Discriminated[A, D]` for some discriminator type `D`, which provides the `Codec[D]` to use for encoding/decoding
 *     the discriminator
 *  - `Discriminator[A, X, D]` for each subtype `X` of `A`, which provides the discriminator value for type `X`
 *  - `Codec[X]` for each subtype `X` of `A`
 *
 * Full examples are available in the test directory of this project.
 *
 * Note that both case class and sealed hierarchies require implicit component codecs in scope. In both cases,
 * those implicit codecs can themselves be automatically derived, although diverging implicit expansion
 * errors often occur when recursively deriving codecs. These errors can be avoided by lifting derived
 * codecs for the component types to implicit codecs like so: {{{
 case class Foo(x: Bar, y: Baz, ...)
 implicit val codecBar = Codec.derive[Bar]
 implicit val codecBaz = Codec.derive[Baz]
 Codec.derive[Foo] }}}
 *
 * === Implicit Codecs ===
 *
 * Codecs derived automatically are not defined implicitly -- meaning that if `Codec.derive[Foo]` returns
 * a derived codec, that derived codec will not be available via `implicitly[Codec[Foo]]`. Instead,
 * derived codecs are provided implicitly via the [[DerivedCodec]] witness. In fact, `Codec.derive[A]`
 * is just an implicit summoning method for `DerivedCodec[A]`.
 *
 * When writing generic combinators that depend on implicitly available codecs, it is often useful
 * to allow for fallback to a derived codec if there is no explicitly defined implicit codec available.
 * This support is provided by the [[ImplicitCodec]] witness. Instead of requesting an implicit `Codec[A]`,
 * request an `ImplicitCodec[A]` to get the fallback to derived behavior.
 *
 * == Miscellaneous ==
 *
 * Note: the decode function can be lifted to a state action via `StateT[Err \/ ?, BitVector, A]`. This type alias
 * and associated constructor is provided by `[[DecodingContext]]`.
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
   * The isomorphism is provided by the implicit `CodecAs` instance.
   *
   * Typically used when `B` is a case class and `A` is an `HList` with the same shape as the elements of `B`.
   *
   * @group hlist
   */
  final def as[B](implicit as: CodecAs[B, A]): Codec[B] = as(this)

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
   * Creates a new codec that is functionally equivalent to this codec but pushes the specified
   * context string in to any errors returned from encode or decode.
   * @group combinators
   */
  final def withContext(context: String): Codec[A] = new Codec[A] {
    override def encode(a: A) = self.encode(a).leftMap { _ pushContext context }
    override def decode(buffer: BitVector) = self.decode(buffer).leftMap { _ pushContext context }
    override def toString = s"$context($self)"
  }

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
  def :+:[B](left: Codec[B]): codecs.CoproductCodecBuilder[B :+: A :+: CNil, Codec[B] :: Codec[A] :: HNil, B :+: A :+: CNil] =
    codecs.CoproductCodecBuilder(left :: self :: HNil)

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
abstract class CodecAs[B, A] {
  def apply(ca: Codec[A]): Codec[B]
}

/** Companion for [[CodecAs]]. */
object CodecAs {

  /** Provides a `CodecAs[B, A]` for case class `B` and HList `A`. */
  implicit def mkAs[B, Repr, A](implicit gen: Generic.Aux[B, Repr], aToR: A =:= Repr, rToA: Repr =:= A): CodecAs[B, A]  = new CodecAs[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.from(a)
      val to: B => A = b => gen.to(b)
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAs[B, A]` for HList `B` and case class `A`. */
  implicit def mkAsReverse[B, Repr, A](implicit gen: Generic.Aux[A, Repr], bToR: B =:= Repr, rToB: Repr =:= B): CodecAs[B, A]  = new CodecAs[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.to(a)
      val to: B => A = b => gen.from(b)
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAs[B, A]` for singleton case class `B` and value `A`. */
  implicit def mkAsSingleton[B, Repr, A](implicit gen: Generic.Aux[B, Repr], aToR: (A :: HNil) =:= Repr, rToA: Repr =:= (A :: HNil)): CodecAs[B, A]  = new CodecAs[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.from(a :: HNil)
      val to: B => A = b => gen.to(b).head
      ca.xmap(from, to)
    }
  }

  /** Provides a `CodecAs[B, A]` for value `B` and singleton case class `A`. */
  implicit def mkAsSingletonReverse[B, Repr, A](implicit gen: Generic.Aux[A, Repr], bToR: (B :: HNil) =:= Repr, rToB: Repr =:= (B :: HNil)): CodecAs[B, A]  = new CodecAs[B, A] {
    def apply(ca: Codec[A]): Codec[B] = {
      val from: A => B = a => gen.to(a).head
      val to: B => A = b => gen.from(b :: HNil)
      ca.xmap(from, to)
    }
  }
}

/**
 * Companion for [[Codec]].
 *
 * @groupname ctor Constructors
 * @groupprio ctor 1
 *
 * @groupname conv Conveniences
 * @groupprio conv 2
 *
 * @groupname inst Typeclass Instances
 * @groupprio inst 3
 */
object Codec extends EncoderFunctions with DecoderFunctions {

  /**
   * Creates a codec from encoder and decoder functions.
   * @group ctor
   */
  def apply[A](encoder: A => Err \/ BitVector, decoder: BitVector => Err \/ (BitVector, A)): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder(a)
    override def decode(bits: BitVector) = decoder(bits)
  }

  /**
   * Creates a codec from an encoder and a decoder.
   * @group ctor
   */
  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A] {
    override def encode(a: A) = encoder.encode(a)
    override def decode(bits: BitVector) = decoder.decode(bits)
  }

  /**
   * Gets an implicitly available codec for type `A` -- either an explicitly defined implicit or a derived codec.
   * See [[derive]] for more information on derived codecs.
   * @group ctor
   */
  def apply[A](implicit c: ImplicitCodec[A]): Codec[A] = c.codec

  /**
   * Gets an implicitly available codec for type `A` -- the codec is guaranteed to not be derived.
   * @group ctor
   */
  def nonDerived[A](implicit c: Codec[A]): Codec[A] = c

  /**
   * Derives a codec for the specified type.
   *
   * Codecs can be derived for:
   *  - case classes (and hlists and records), where each component type of the case class either has an
   *    implicitly available codec or one can be automatically derived
   *  - sealed class hierarchies (and coproducts and unions), where:
   *    - the root type, `A`, has an implicitly available `Discriminated[A, D]` for some `D`
   *    - each subtype has an implicitly available codec or can have one derived
   *    - each subtype `X` has an implicitly available `Discriminator[A, X, D]`
   *
   * @group ctor
   */
  def derive[A](implicit d: DerivedCodec[A]): Codec[A] = d.codec

  /**
   * Alias for [[derive]].
   * @group ctor
   */
  @deprecated("As of 1.5, this method is redundant with Codec.derive.", "1.5")
  def product[A](implicit d: DerivedCodec[A]): Codec[A] = d.codec

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
   * @group ctor
   */
  def coproduct[A](implicit auto: codecs.CoproductBuilderAuto[A]): auto.Out = auto.apply

  /**
   * Invariant functor typeclass instance.
   * @group inst
   */
  val invariantFunctorInstance: InvariantFunctor[Codec] = new InvariantFunctor[Codec] {
    def xmap[A, B](c: Codec[A], f: A => B, g: B => A) = c.xmap(f, g)
  }
}
