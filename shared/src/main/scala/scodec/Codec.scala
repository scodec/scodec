package scodec

import scala.deriving._
import scala.compiletime._

import scodec.bits.{BitVector, ByteVector}
import scala.collection.mutable

/**
  * Supports encoding a value of type `A` to a `BitVector` and decoding a `BitVector` to a value of `A`.
  *
  * Not every value of `A` can be encoded to a bit vector and similarly, not every bit vector can be decoded to a value
  * of type `A`. Hence, both encode and decode return either an error or the result. Furthermore, decode returns the
  * remaining bits in the bit vector that it did not use in decoding.
  *
  * There are various ways to create instances of `Codec`. The trait can be implemented directly or one of the
  * constructor methods in the companion can be used (e.g., `apply`). Most of the methods on `Codec`
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
  * In this case, there are three components, each of type `Int`, so the compiler first looked for an implicit `Codec[Int]`.
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
  * @groupname tuple Tuple Support
  * @groupprio tuple 11
  *
  * @groupname hlist HList Support
  * @groupprio hlist 12
  *
  * @groupname generic Generic Support
  * @groupprio generic 13
  *
  * @define TransformTC Codec
  */
trait Codec[A] extends GenCodec[A, A] { self =>

  /**
    * Transforms using two functions, `A => Attempt[B]` and `B => Attempt[A]`.
    * @group combinators
    */
  final def exmap[B](f: A => Attempt[B], g: B => Attempt[A]): Codec[B] = new Codec[B] {
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = self.econtramap(g).encode(b)
    def decode(buffer: BitVector) = self.emap(f).decode(buffer)
  }

  /**
    * Transforms using the isomorphism described by two functions, `A => B` and `B => A`.
    * @group combinators
    */
  final def xmap[B](f: A => B, g: B => A): Codec[B] = new Codec[B] {
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = self.encode(g(b))
    def decode(buffer: BitVector) = self.decode(buffer).map { _.map(f) }
  }

  /**
    * Lifts this codec in to a codec of a singleton tuple.
    * @group tuple
    */
  final def tuple: Codec[A *: Unit] = xmap(_ *: (), _.head)

  /**
    * Assuming `A` is `Unit`, creates a `Codec[B]` that: encodes the unit followed by a `B`;
    * decodes a unit followed by a `B` and discards the decoded unit.
    *
    * @group tuple
    */
  final def dropLeft[B](codecB: Codec[B])(implicit ev: Unit =:= A): Codec[B] =
    (this :: codecB).xmap[B]({ (_, b) => b }, b => (ev(()), b))

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
   (this :: codecB).xmap[A]({ (a, _) => a }, a => (a, ev(())))

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
    * Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
    * @group tuple
    */
  final def flatZip[B](f: A => Codec[B]): Codec[(A, B)] = new Codec[(A, B)] {
    def sizeBound: SizeBound = self.sizeBound.atLeast
    override def encode(t: (A, B)) = Codec.encodeBoth(self, f(t._1))(t._1, t._2)
    override def decode(buffer: BitVector) =
      (for {
        a <- self
        b <- f(a)
      } yield (a, b)).decode(buffer)
  }

  /**
    * Returns a new codec that encodes/decodes a value of type `(A, B)` where the codec of `B` is dependent on `A`.
    * Operator alias for [[flatZip]].
    * @group tuple
    */
  final def >>~[B](f: A => Codec[B]): Codec[(A, B)] = flatZip(f)

  /**
    * Similar to `flatZip` except the `A` type is not visible in the resulting type -- the binary
    * effects of the `Codec[A]` still occur though.
    *
    * Example usage: {{{
     case class Flags(x: Boolean, y: Boolean, z: Boolean)
     (bool :: bool :: bool :: ignore(5)).consume { flgs =>
       conditional(flgs.x, uint8) :: conditional(flgs.y, uint8) :: conditional(flgs.z, uint8)
     } {
       case x :: y :: z :: HNil => Flags(x.isDefined, y.isDefined, z.isDefined) }
     }
   }}}
    *
    * Note that when `B` is an `HList`, this method is equivalent to using `flatPrepend` and
    * `derive`. That is,
    * `a.consume(f)(g) === a.flatPrepend(f).derive[A].from(g)`.
    *
    * @group combinators
    */
  final def consume[B](f: A => Codec[B])(g: B => A): Codec[B] = new Codec[B] {
    def sizeBound = self.sizeBound.atLeast
    def encode(b: B) = {
      val a = g(b)
      for {
        encA <- self.encode(a)
        encB <- f(a).encode(b)
      } yield encA ++ encB
    }
    def decode(bv: BitVector) =
      (for {
        a <- self
        b <- f(a)
      } yield b).decode(bv)
  }

  final override def complete: Codec[A] = Codec(this, super.complete)

  final override def compact: Codec[A] = Codec(super.compact, this)

  /**
    * Safely lifts this codec to a codec of a supertype.
    *
    * When a subtype of `B` that is not a subtype of `A` is passed to encode,
    * an encoding error is returned.
    *
    * @group combinators
    */
  final def upcast[B >: A](implicit ct: reflect.ClassTag[A]): Codec[B] = new Codec[B] {
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = b match {
      case a: A => self.encode(a)
      case _    => Attempt.failure(Err(s"not a value of type ${ct.runtimeClass.getSimpleName}"))
    }
    def decode(bv: BitVector) = self.decode(bv)
    override def toString = self.toString
  }

  /**
    * Safely lifts this codec to a codec of a subtype.
    *
    * When a supertype of `B` that is not a supertype of `A` is decoded,
    * an decoding error is returned.
    *
    * @group combinators
    */
  final def downcast[B <: A](implicit ct: reflect.ClassTag[B]): Codec[B] = new Codec[B] {
    def sizeBound: SizeBound = self.sizeBound
    def encode(b: B) = self.encode(b)
    def decode(bv: BitVector) = self.decode(bv).flatMap { result =>
      result.value match {
        case b: B => Attempt.successful(DecodeResult(b, result.remainder))
        case _    => Attempt.failure(Err(s"not a value of type ${ct.runtimeClass.getSimpleName}"))
      }
    }
    override def toString = self.toString
  }

  /**
    * Creates a new codec that is functionally equivalent to this codec but pushes the specified
    * context string in to any errors returned from encode or decode.
    * @group combinators
    */
  final def withContext(context: String): Codec[A] = new Codec[A] {
    def sizeBound: SizeBound = self.sizeBound
    override def encode(a: A) = self.encode(a).mapErr { _.pushContext(context) }
    override def decode(buffer: BitVector) = self.decode(buffer).mapErr { _.pushContext(context) }
    override def toString = s"$context($self)"
  }

  /**
    * Creates a new codec that is functionally equivalent to this codec but returns the specified string from `toString`.
    * @group combinators
    */
  final def withToString(str: => String): Codec[A] = new Codec[A] {
    override def sizeBound: SizeBound = self.sizeBound
    override def encode(a: A) = self.encode(a)
    override def decode(buffer: BitVector) = self.decode(buffer)
    override def toString = str
  }

  // /**
  //   * Supports creation of a coproduct codec. See [[scodec.codecs.CoproductCodecBuilder]] for details.
  //   * @group coproduct
  //   */
  // def :+:[B](
  //     left: Codec[B]
  // ): codecs.CoproductCodecBuilder[B :+: A :+: CNil, Codec[B] :: Codec[A] :: HNil, B :+: A :+: CNil] =
  //   codecs.CoproductCodecBuilder(left :: self :: HNil)

  // /**
  //   * Lifts this codec to a codec of a shapeless field -- allowing it to be used in records and unions.
  //   * @group combinators
  //   */
  // def toField[K]: Codec[FieldType[K, A]] =
  //   xmap[FieldType[K, A]](a => labelled.field[K](a), identity)

  // /**
  //   * Lifts this codec to a codec of a shapeless field -- allowing it to be used in records and unions.
  //   * The specified key is pushed in to the context of any errors that are returned from the resulting codec.
  //   * @group combinators
  //   */
  // def toFieldWithContext[K <: Symbol](k: K): Codec[FieldType[K, A]] =
  //   toField[K].withContext(k.name)

  override def decodeOnly[AA >: A]: Codec[AA] = {
    val sup = super.decodeOnly[AA]
    new Codec[AA] {
      def sizeBound = self.sizeBound
      def encode(a: AA) = sup.encode(a)
      def decode(bits: BitVector) = sup.decode(bits)
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
  * @groupname inst Supporting Instances
  * @groupprio inst 3
  */
object Codec extends EncoderFunctions with DecoderFunctions {

  /**
    * Creates a codec from encoder and decoder functions.
    * @group ctor
    */
  def apply[A](
      encoder: A => Attempt[BitVector],
      decoder: BitVector => Attempt[DecodeResult[A]]
  ): Codec[A] = new Codec[A] {
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(a: A) = encoder(a)
    override def decode(bits: BitVector) = decoder(bits)
  }

  /**
    * Creates a codec from an encoder and a decoder.
    * @group ctor
    */
  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A] {
    override def sizeBound: SizeBound = encoder.sizeBound
    override def encode(a: A) = encoder.encode(a)
    override def decode(bits: BitVector) = decoder.decode(bits)
  }

  /**
    * Provides a `Codec[A]` that delegates to a lazily evaluated `Codec[A]`.
    * Typically used to consruct codecs for recursive structures.
    *
    * @group ctor
    */
  def lazily[A](codec: => Codec[A]): Codec[A] = new Codec[A] {
    // Avoid use of lazy val to avoid synchronization - okay to race on creation of codec,
    // and okay for multiple threads to compute their own codecs
    private var _c: Codec[A] = null
    private def c: Codec[A] = {
      if (_c eq null) {
        _c = codec
        if (_c eq null) throw new NullPointerException("lazy codec constructor returned null")
      }
      _c
    }
    def sizeBound = c.sizeBound
    def encode(a: A) = c.encode(a)
    def decode(b: BitVector) = c.decode(b)
    override def toString = s"lazily($c)"
  }

  extension on [T <: Tuple, U <: Tuple](t: Codec[T])(given u: codecs.DropUnits[T] { type L = U }) {
    def dropUnits: Codec[U] = t.xmap(u.removeUnits, u.addUnits)
  }

  extension on [H, T <: Tuple](t: Codec[T]) {
    /**
      * Builds a `Codec[H *: T]` from a `Codec[H]` and a `Codec[T]` where `T` is a tuple type.
      * That is, this operator is a codec-level tuple prepend operation.
      * @param codec codec to prepend
      * @group tuple
      */
    def ::(h: Codec[H]): Codec[H *: T] =
      new Codec[H *: T] {
        def sizeBound = h.sizeBound + t.sizeBound
        def encode(ht: H *: T) = encodeBoth(h, t)(ht.head, ht.tail)
        def decode(bv: BitVector) = decodeBoth(h, t)(bv).map(_.map(_ *: _))
        override def toString = s"$h :: $t"
      } 
  }

  extension on [A, B](b: Codec[B]) {
    /**
      * When called on a `Codec[A]` where `A` is not a tuple, creates a new codec that encodes/decodes a tuple of `(B, A)`.
      * For example, {{{uint8 :: utf8}}} has type `Codec[(Int, Int)]`.
      * @group tuple
      */
    def ::(a: Codec[A]): Codec[(A, B)] =
      new Codec[(A, B)] {
        def sizeBound = a.sizeBound + b.sizeBound
        def encode(ab: (A, B)) = Codec.encodeBoth(a, b)(ab._1, ab._2)
        def decode(bv: BitVector) = Codec.decodeBoth(a, b)(bv)
        override def toString = s"$a :: $b"
      }
  }

  extension on [A, B <: Tuple](codecA: Codec[A]) {
    /**
      * Creates a new codec that encodes/decodes a tuple of `A :: B` given a function `A => Codec[B]`.
      * This allows later parts of a tuple codec to be dependent on earlier values.
      * @group tuple
      */
    def flatPrepend(f: A => Codec[B]): Codec[A *: B] =
      new Codec[A *: B] {
        def sizeBound = codecA.sizeBound.atLeast
        def encode(ab: A *: B) = encodeBoth(codecA, f(ab.head))(ab.head, ab.tail)
        def decode(b: BitVector) =
          (for {
            a <- codecA
            l <- f(a)
          } yield a *: l).decode(b)
        override def toString = s"flatPrepend($codecA, $f)"
      }
      
    /**
      * Creates a new codec that encodes/decodes a tuple of `A :: B` given a function `A => Codec[B]`.
      * This allows later parts of a tuple codec to be dependent on earlier values.
      * Operator alias for `flatPrepend`.
      * @group tuple
      */
    def >>:~(f: A => Codec[B]): Codec[A *: B] = codecA.flatPrepend(f)
  }

  extension on [B <: Tuple](rhs: Codec[B]) {
    /**
      * When called on a `Codec[L]` for some `L <: HList`, returns a new codec that encodes/decodes
      * `B :: L` but only returns `L`.  HList equivalent of `~>`.
      * @group hlist
      */
    def :~>:(lhs: Codec[Unit]): Codec[B] = lhs.dropLeft(rhs)
  }
  //   /**
  //     * When called on a `Codec[A]`, returns a new codec that encodes/decodes `B :: A :: HNil`.
  //     * HList equivalent of `~>`.
  //     * @group hlist
  //     */
  //   def :~>:[B](codecB: Codec[B])(implicit ev: Unit =:= B): Codec[A :: HNil] =
  //     codecB :~>: self.hlist





  // def [H, T <: Tuple] (h: Codec[H]) :: (t: Codec[T]): Codec[H *: T] =
  //  new Codec[H *: T] {
  //     def sizeBound = h.sizeBound + t.sizeBound
  //     def encode(ht: H *: T) = Codec.encodeBoth(h, t)(ht.head, ht.tail)
  //     def decode(bv: BitVector) = Codec.decodeBoth(h, t)(bv).map(_.map(_ *: _))
  //     override def toString = s"$h :: $t"
  //   } 

  // def [A, B] (a: Codec[A]) :: (b: Codec[B])(given DummyImplicit): Codec[(A, B)] =
  //   new Codec[(A, B)] {
  //     def sizeBound = a.sizeBound + b.sizeBound
  //     def encode(ab: (A, B)) = Codec.encodeBoth(a, b)(ab._1, ab._2)
  //     def decode(bv: BitVector) = Codec.decodeBoth(a, b)(bv)
  //     override def toString = s"$a :: $b"
  //   }

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
  // def coproduct[A](implicit auto: codecs.CoproductBuilderAuto[A]): auto.Out = auto.apply

  inline given derivedTuple[T <: Tuple] as Codec[T] = {
    val codecs = summonCodecs[T].toArray.asInstanceOf[Array[Codec[_]]]
    deriveProduct(codecs, _.toArray.iterator, p => Tuple.fromProduct(p).asInstanceOf[T])
  }

  inline def derived[A](given m: Mirror.Of[A]): Codec[A] = {
    val elemCodecs = summonCodecs[m.MirroredElemTypes].toArray.asInstanceOf[Array[Codec[_]]]
    val elemLabels = summonLabels[m.MirroredElemLabels]
    val codecs = elemCodecs.zip(elemLabels).map { (c, l) =>
      c.withContext(l).asInstanceOf[Codec[_]]
    }
    inline m match {
      case p: Mirror.ProductOf[A] =>
        deriveProduct(codecs, a => a.asInstanceOf[Product].productIterator, t => p.fromProduct(t).asInstanceOf[A])
      case s: Mirror.SumOf[A] =>
        deriveSum(s, codecs)
    }
  }

  private inline def summonOne[A]: A = summonFrom { case a: A => a }

  private inline def summonCodecs[T <: Tuple]: List[Codec[_]] = inline erasedValue[T] match {
    case _: Unit => Nil
    case _: (t *: ts) => summonOne[Codec[t]] :: summonCodecs[ts]
  }

  private inline def summonLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: Unit => Nil
    case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabels[ts]
  }

  private def deriveProduct[A](codecs: Array[Codec[_]], toElems: A => Iterator[Any], mk: Product => A): Codec[A] = {
    new Codec[A] {
      def sizeBound = codecs.foldLeft(SizeBound.exact(0))(_ + _.sizeBound)
      def encode(a: A) = {
        var i = 0
        val elems = toElems(a)
        var result = BitVector.empty
        var err: Err = null
        while (i < codecs.size && (err eq null)) {
          val elem = elems.next.asInstanceOf[Any]
          val codec = codecs(i).asInstanceOf[Codec[Any]]
          codec.encode(elem) match {
            case Attempt.Successful(out) =>
              result = result ++ out
            case Attempt.Failure(e) => err = e
          }
          i += 1
        }
        if (err eq null) Attempt.successful(result) else Attempt.failure(err)
      }
      def decode(b: BitVector) = {
        var i = 0
        val bldr = mutable.ArrayBuilder.make[AnyRef]
        var buf = b
        var err: Err = null
        while (i < codecs.size && (err eq null)) {
          val codec = codecs(i).asInstanceOf[Codec[AnyRef]]
          codec.decode(buf) match {
            case Attempt.Successful(DecodeResult(a, rem)) =>
              bldr += a
              buf = rem
            case Attempt.Failure(e) => err = e
          }
          i += 1
        }
        if (err eq null) Attempt.successful(DecodeResult(mk(new ArrayProduct(bldr.result)), buf))
        else Attempt.failure(err)
      }
    }
  }

  private def deriveSum[A](
    s: Mirror.SumOf[A], 
    elemCodecs: Array[Codec[_]],
  ): Codec[A] = new Codec[A] {
    private val discriminator = codecs.uint8
    def sizeBound = discriminator.sizeBound + SizeBound.choice(elemCodecs.map(_.sizeBound))
    def encode(a: A) = {
      val idx = s.ordinal(a)
      (discriminator :: elemCodecs(idx).asInstanceOf[Codec[A]]).encode(idx, a)
    }
    def decode(b: BitVector) = {
      discriminator.flatMap(idx => elemCodecs(idx).asInstanceOf[Codec[A]]).decode(b)
    }
  }

  inline given derivedSingleton[A <: Singleton](given m: Mirror.Of[A]) as Codec[A] = {
    inline m match {
      case s: Mirror.Singleton => codecs.provide(s.fromProduct(null).asInstanceOf[A])
    }
  }

  given Codec[Byte] = codecs.byte
  given Codec[Short] = codecs.short16
  given Codec[Int] = codecs.int32
  given Codec[Long] = codecs.int64
  given Codec[Float] = codecs.float
  given Codec[Double] = codecs.double
  given Codec[String] = codecs.utf8_32
  given Codec[Boolean] = codecs.bool(8)
  given Codec[BitVector] = codecs.variableSizeBitsLong(codecs.int64, codecs.bits)
  given Codec[ByteVector] = codecs.variableSizeBytesLong(codecs.int64, codecs.bytes)
  given Codec[java.util.UUID] = codecs.uuid

  given [A](given ccount: Codec[Int], ca: Codec[A]) as Codec[List[A]] = codecs.listOfN(ccount, ca)
  given [A](given ccount: Codec[Int], ca: Codec[A]) as Codec[Vector[A]] = codecs.vectorOfN(ccount, ca)
  given [A](given cguard: Codec[Boolean], ca: Codec[A]) as Codec[Option[A]] = codecs.optional(cguard, ca)

  given Transform[Codec] {
    def [A, B](fa: Codec[A]).exmap(f: A => Attempt[B], g: B => Attempt[A]): Codec[B] = 
      fa.exmap(f, g)
  }
}
