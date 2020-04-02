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
  * The `::` operator supports combining a `Codec[A]` and a `Codec[B]` in to a `Codec[(A, B)]`.
  *
  * For example: {{{
   val codec: Codec[(Int, Int, Int)] = uint8 :: uint8 :: uint8}}}
 }}}
  *
  * There are various methods on `Codec` that only work on `Codec[A]` for some `A <: Tuple`. Besides the aforementioned
  * `::` method, they include methods like `++`, `flatPrepend`, `flatConcat`, etc. One particularly useful method is
  * `dropUnits`, which removes any `Unit` values from the tuple.
  *
  * Given a `Codec[(X0, X1, ..., Xn)]` and a case class with types `X0` to `Xn` in the same order,
  * the codec can be turned in to a case class codec via the `as` method. For example:
 {{{
  case class Point(x: Int, y: Int, z: Int)
  val threeInts: Codec[(Int, Int, Int)] = uint8 :: uint8 :: uint8
  val point: Codec[Point] = threeInts.as[Point]
 }}}
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
  val x: Codec[(Int, ByteVector)] = uint8.flatZip { numBytes => bytes(numBytes) }
  val y: Codec[ByteVector] = x.xmap[ByteVector]({ case (_, bv) => bv }, bv => (bv.size, bv))
 }}}
  * In this example, `x` is a `Codec[(Int, ByteVector)]` but we do not need the size directly in the model
  * because it is redundant with the size stored in the `ByteVector`. Hence, we remove the `Int` by
  * `xmap`-ping over `x`. The notion of removing redundant data from models comes up frequently.
  * Note: there is a combinator that expresses this pattern more succinctly -- `variableSizeBytes(uint8, bytes)`.
  *
 *
  * === flatPrepend ===
  *
  * When the function passed to `flatZip` returns a `Codec[B]` where `B <: Tuple`, you end up creating
  * right nested tuples instead of a extending the arity of a single tuple. To do the latter, there's
  * `flatPrepend`. It has the signature: {{{
  def flatPrepend[B <: Tuple](f: A => Codec[B]): Codec[A *: B]
 }}}
  * It forms a codec of `A` consed on to `B` when called on a `Codec[A]` and passed a function `A => Codec[B]`.
  * Note that the specified function must return a tuple codec. Implementing our example from earlier
  * using `flatPrepend`: {{{
  val x: Codec[(Int, ByteVector)] = uint8.flatPrepend { numBytes => bytes(numBytes).tuple }
 }}}
  * In this example, `bytes(numBytes)` returns a `Codec[ByteVector]` so we called `.tuple` on it to lift it
  * in to a `Codec[ByteVector *: Unit]`.
  *
  * There are similar methods for flat appending and flat concating.
  *
  * == Derived Codecs ==
  *
  * Codecs for case classes and sealed class hierarchies can often be automatically derived.
  *
  * Consider this example: {{{
  case class Point(x: Int, y: Int, z: Int) derives Codec
  Codec[Point].encode(Point(1, 2, 3))
 }}}
  * In this example, no explicit codec was defined for `Point` and instead, an implicit one was derived as a result
  * of the `derives Codec` clause. Derivation of a codec for a case class requires each element of the case class to
  * have an implicitly available codec of the corresponding type. In this case, each element was an `Int` and there is
  * an implicit `Codec[Int]` in the companion of `Codec`.
  * 
  * Derived codecs include the name of each element in any errors produced when encoding/decoding the element.
  *
  * This works similarly for ADTs / sealed class hierarchies. The binary form is represented as a single
  * unsigned 8-bit integer representing the ordinal of the sum, followed by the derived form of the product.
  *
  * Full examples are available in the test directory of this project.
  *
  * @groupname tuple Tuple Support
  * @groupprio tuple 11
  */
trait Codec[A] extends Encoder[A] with Decoder[A] { self =>

  /**
   * Transforms this codec to a `Codec[B]` if `A` is isomorphic to `B`.
   * 
   * This is most commonly used to convert a tuple codec to a case class:
   * @example {{{
   * case class Point(x: Int, y: Int, z: Int)
   * val c: Codec[(Int, Int, Int)] = int8 :: int8 :: int8
   * val p: Codec[Point] = c.as[Point]
   * }}}
   */
  def as[B](using iso: Iso[A, B]): Codec[B] = xmap(iso.to, iso.from)

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
    def decode(buffer: BitVector) = self.decode(buffer).map(_.map(f))
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
       case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
     }
   }}}
    *
    * Note that when `B` is a tuple, this method is equivalent to using `flatPrepend` and
    * `deriveElement`. That is,
    * `a.consume(f)(g) === a.flatPrepend(f).deriveElement(g)`.
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
    override def encode(a: A) = self.encode(a).mapErr(_.pushContext(context))
    override def decode(buffer: BitVector) = self.decode(buffer).mapErr(_.pushContext(context))
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

  inline def apply[A](using c: Codec[A]): Codec[A] = c

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
    @annotation.threadUnsafe lazy val c: Codec[A] = codec
    def sizeBound = c.sizeBound
    def encode(a: A) = c.encode(a)
    def decode(b: BitVector) = c.decode(b)
    override def toString = s"lazily($c)"
  }

  extension tupleOpsNoParams on [A <: Tuple](codecA: Codec[A]) {
    inline def dropUnits: Codec[DropUnits.T[A]] =
      codecA.xmap(a => DropUnits.drop(a), b => DropUnits.insert(b))
  }

  extension tupleOpsLeftAssociative on [A <: Tuple, B](codecA: Codec[A]) {
    /**
      * When called on a `Codec[A]` for some `A <: Tuple`, returns a new codec that encodes/decodes
      * the tuple `A` followed by the value `B`, where the latter is encoded/decoded with the codec
      * returned from applying `A` to `f`.
      * @group tuple
      */
    inline def flatAppend(f: A => Codec[B]): Codec[Tuple.Concat[A, B *: Unit]] = new Codec[Tuple.Concat[A, B *: Unit]] {
      def sizeBound = codecA.sizeBound.atLeast
      def encode(ab: Tuple.Concat[A, B *: Unit]) = {
        val size = constValue[Tuple.Size[A]]
        val (a, b) = ab.splitAt(size).asInstanceOf[(A, B *: Unit)]
        encodeBoth(codecA, f(a))(a, b.head)
      }
      def decode(bv: BitVector) =
        codecA.decode(bv).flatMap { case DecodeResult(a, rem) =>
          f(a).decode(rem).map(_.map(b => (a ++ (b *: ())): Tuple.Concat[A, B *: Unit]))
          // FIXME cast due to https://github.com/lampepfl/dotty/issues/8321
        }
    }
  }

  extension tupleOpsRightAssociative on [A, B <: Tuple](codecB: Codec[B]) {
    /**
      * Builds a `Codec[A *: B]` from a `Codec[A]` and a `Codec[B]` where `B` is a tuple type.
      * That is, this operator is a codec-level tuple prepend operation.
      * @param codec codec to prepend
      * @group tuple
      */
    def ::(codecA: Codec[A]): Codec[A *: B] =
      new Codec[A *: B] {
        def sizeBound = codecA.sizeBound + codecB.sizeBound
        def encode(ab: A *: B) = encodeBoth(codecA, codecB)(ab.head, ab.tail)
        def decode(bv: BitVector) = decodeBoth(codecA, codecB)(bv).map(_.map(_ *: _))
        override def toString = s"$codecA :: $codecB"
      }

    /**
      * `codecB :+ codecA` returns a new codec that encodes/decodes the tuple `B` followed by an `A`.
      * That is, this operator is a codec-level tuple append operation.
      * @group tuple
      */
    inline def :+(codecA: Codec[A]): Codec[Tuple.Concat[B, A *: Unit]] = 
      codecB ++ codecA.tuple
  }

  extension tupleBinaryOps on [A <: Tuple, B <: Tuple](codecA: Codec[A]) {
    /**
      * Builds a `Codec[A ++ B]` from a `Codec[A]` and a `Codec[B]` where `A` and `B` are tuples.
      * That is, this operator is a codec-level tuple concat operation.
      * @param codecA codec to concat
      * @group tuple
      */
    inline def ++(codecB: Codec[B]): Codec[Tuple.Concat[A, B]] =
      new Codec[Tuple.Concat[A, B]] {
        def sizeBound = codecA.sizeBound + codecB.sizeBound
        def encode(ab: Tuple.Concat[A, B]) = {
          inline val sizeA = constValue[Tuple.Size[A]]
          val (prefix, suffix) = ab.splitAt(sizeA)
          encodeBoth(codecA, codecB)(prefix.asInstanceOf[A], suffix.asInstanceOf[B])
        }
        def decode(bv: BitVector) =
          decodeBoth(codecA, codecB)(bv).map(_.map((a: A, b: B) => (a ++ b).asInstanceOf[Tuple.Concat[A, B]]))
          // FIXME cast due to https://github.com/lampepfl/dotty/issues/8321
        override def toString = s"$codecA :: $codecB"
      } 

    /**
      * When called on a `Codec[A]` for some `A <: Tuple`, returns a new codec that encodes/decodes
      * the tuple `A` followed by the tuple `B`, where the latter is encoded/decoded with the codec
      * returned from applying `A` to `f`.
      * @group tuple
      */
    inline def flatConcat(f: A => Codec[B]): Codec[Tuple.Concat[A, B]] = new Codec[Tuple.Concat[A, B]] {
      def sizeBound = codecA.sizeBound.atLeast
      def encode(ab: Tuple.Concat[A, B]) = {
        val size = constValue[Tuple.Size[A]]
        val (a, b) = ab.splitAt(size).asInstanceOf[(A, B)]
        encodeBoth(codecA, f(a))(a, b)
      }
      def decode(bv: BitVector) =
        codecA.decode(bv).flatMap { case DecodeResult(a, rem) =>
          f(a).decode(rem).map(_.map(b => a ++ b))
        }
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
  }

  implicit class DeriveSyntax[A <: Tuple](private val self: Codec[A]) extends AnyVal {
    /**
      * Supports building a `Codec[C]` where `C` is the tuple that results in removing
      * the first `B` from `A`.
      *
      * Example usage: {{{
       case class Flags(x: Boolean, y: Boolean, z: Boolean)
       val c = (bool :: bool :: bool :: ignore(5)).flatPrepend { flgs =>
         conditional(flgs.x, uint8) :: conditional(flgs.y, uint8) :: conditional(flgs.z, uint8)
       }
       c.deriveElement { case (x, y, z) => Flags(x.isDefined, y.isDefined, z.isDefined) }
     }}}
      *
      * This codec, the `Codec[A]`, is used for encoding/decoding. When decoding, the first value of type
      * `A` is removed from the tuple.
      *
      * When encoding, the returned codec computes a `B` value using the supplied
      * function and inserts the computed `B` in to the tuple `C`, yielding a tuple `A`. That tuple `A`
      * is then encoded using the original codec.
      *
      * This method is called `deriveElement` because the value of type `B` is derived from the other elements
      * of the tuple `A`.
      *
      * @tparam B type to remove from `A` and derive from the remaining elements
      * @group tuple
      */
    inline def deriveElement[B](f: TupleWithout[A, B] => B): Codec[TupleWithout[A, B]] =
      self.xmap(a => remove[A, B](a), c => insert[A, B](c, f(c)))
  }

  private inline def remove[A <: Tuple, B](a: A): TupleWithout[A, B] = {
    val i = constValue[TupleIndexOf[A, B]]
    val prefix = a.take(i)
    val j = constValue[S[TupleIndexOf[A, B]]]
    val suffix = a.drop(j)
    (prefix ++ suffix).asInstanceOf[TupleWithout[A, B]]
  }

  private inline def insert[A <: Tuple, B](c: TupleWithout[A, B], b: B): A = {
    val i = constValue[TupleIndexOf[A, B]]
    val (prefix, suffix) = c.splitAt(i)
    (prefix ++ (b *: suffix)).asInstanceOf[A]
  }

  type TupleWithout[A <: Tuple, B] <: Tuple = A match {
    case hd *: tl => hd match {
      case B => tl
      case _ => hd *: TupleWithout[tl, B]
    }
  }

  type TupleIndexOf[A <: Tuple, B] <: Int = A match {
    case hd *: tl => hd match {
      case B => 0
      case _ => S[TupleIndexOf[tl, B]]
    }
  }

  /**
    * Constructs a `Codec[(A, B, ..., N)]` from a tuple `(Codec[A], Codec[B], ..., Codec[N])`.
    */
  def fromTuple[A <: Tuple : Tuple.IsMappedBy[Codec]](a: A): Codec[Tuple.InverseMap[A, Codec]] = {
    def go[X <: Tuple](x: X): Codec[_ <: Tuple] = x match {
      case (hd: Codec[_]) *: tl => hd :: go(tl)
      case () => codecs.provide(())
    }
    go(a).asInstanceOf[Codec[Tuple.InverseMap[A, Codec]]]
  }

  inline given derivedTuple[A <: Tuple] as Codec[A] = new Codec[A] {
    def sizeBound = sizeBoundElems[A]
    def encode(t: A) = encodeTuple[A](t, 0)
    def decode(b: BitVector) = {
      inline val size = constValue[Tuple.Size[A]]
      decodeTuple[A, A](b, 0, new ArrayProduct(size))
    }
  }

  private inline def encodeTuple[A <: Tuple](a: Any, i: Int): Attempt[BitVector] =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        val hdCodec = summonOne[Codec[hd]]
        hdCodec.encode(productElement[hd](a, i)).flatMap { encHd =>
          encodeTuple[tl](a, i + 1).map { encTl =>
            encHd ++ encTl
          }
        }
      case _: Unit =>
        Attempt.successful(BitVector.empty)
    }

  private inline def decodeTuple[T <: Tuple, A <: Tuple](b: BitVector, i: Int, elems: ArrayProduct): Attempt[DecodeResult[T]] =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        val hdCodec = summonOne[Codec[hd]]
        hdCodec.decode(b).flatMap { case DecodeResult(e, rem) =>
          elems(i) = e
          decodeTuple[T, tl](rem, i + 1, elems)
        }
      case _: Unit =>
        Attempt.successful(DecodeResult(Tuple.fromProduct(elems).asInstanceOf[T], b))
    }

  inline def derived[A](using m: Mirror.Of[A]): Codec[A] = new Codec[A] {
    def sizeBound = inline m match {
      case p: Mirror.ProductOf[A] =>
        sizeBoundElems[p.MirroredElemTypes]
      case s: Mirror.SumOf[A] =>
        codecs.uint8.sizeBound + sizeBoundCases[s.MirroredElemTypes]
    }
    def encode(a: A) = inline m match {
      case p: Mirror.ProductOf[A] =>
        encodeElems[p.MirroredElemTypes, p.MirroredElemLabels](a, 0)
      case s: Mirror.SumOf[A] =>
        val ordinal = s.ordinal(a)
        codecs.uint8.encode(ordinal).flatMap { enc =>
          encodeCases[s.MirroredElemTypes, s.MirroredElemLabels](a, ordinal, 0).map(enc2 => enc ++ enc2)
        }
    }
    def decode(b: BitVector) = inline m match {
      case p: Mirror.ProductOf[A] =>
        inline val size = constValue[Tuple.Size[p.MirroredElemTypes]]
        val elems = new ArrayProduct(size)
        decodeElems[p.MirroredElemTypes, p.MirroredElemLabels](b, 0, elems).map { case DecodeResult(_, rem) =>
          DecodeResult(p.fromProduct(elems), rem)
        }
      case s: Mirror.SumOf[A] =>
        codecs.uint8.decode(b).flatMap { case DecodeResult(ordinal, rem) =>
          decodeCases[A, s.MirroredElemTypes, s.MirroredElemLabels](rem, ordinal, 0)
        }
    }
  }

  private inline def sizeBoundElems[A <: Tuple]: SizeBound =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        summonOne[Codec[hd]].sizeBound + sizeBoundElems[tl]
      case _: Unit =>
        SizeBound.exact(0)
    }

  private inline def sizeBoundCases[A <: Tuple]: SizeBound =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        val hdSize = summonFrom {
          case p: Mirror.ProductOf[`hd`] =>
            sizeBoundElems[p.MirroredElemTypes]
        }
        hdSize | sizeBoundCases[tl]
      case _: Unit =>
        SizeBound.exact(0)
    }

  private inline def encodeElems[A <: Tuple, L <: Tuple](a: Any, i: Int): Attempt[BitVector] =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdCodec = summonOne[Codec[hd]].withContext(hdLabelValue)
            hdCodec.encode(productElement[hd](a, i)).flatMap { encHd =>
              encodeElems[tl, tlLabels](a, i + 1).map { encTl =>
                encHd ++ encTl
              }
            }
          case _: Unit => sys.error("not possible - label for product not available")
        }
      case _: Unit =>
        Attempt.successful(BitVector.empty)
    }

  private inline def encodeCases[A <: Tuple, L <: Tuple](a: Any, ordinal: Int, i: Int): Attempt[BitVector] =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            if (ordinal == i) {
              val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
              summonFrom {
                case p: Mirror.ProductOf[`hd`] =>
                  encodeElems[p.MirroredElemTypes, p.MirroredElemLabels](a, 0).mapErr(_.pushContext(hdLabelValue))
              }
            } else encodeCases[tl, tlLabels](a, ordinal, i + 1)
          case _: Unit => sys.error("not possible - label for product not available")
        }
      case _: Unit => Attempt.successful(BitVector.empty)
    }

  private inline def decodeElems[A <: Tuple, L <: Tuple](b: BitVector, i: Int, elems: ArrayProduct): Attempt[DecodeResult[Any]] =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
            val hdCodec = summonOne[Codec[hd]].withContext(hdLabelValue)
            hdCodec.decode(b).flatMap { case DecodeResult(e, rem) =>
              elems(i) = e
              decodeElems[tl, tlLabels](rem, i + 1, elems)
            }
          case _: Unit =>
            sys.error("not possible - label for product not available")
        }
      case _: Unit =>
        Attempt.successful(DecodeResult((), b))
    }

  private inline def decodeCases[S, A <: Tuple, L <: Tuple](b: BitVector, ordinal: Int, i: Int): Attempt[DecodeResult[S]] =
    inline erasedValue[A] match {
      case _: (hd *: tl) =>
        inline erasedValue[L] match {
          case _: (hdLabel *: tlLabels) =>
            if (ordinal == i) {
              summonFrom {
                case s: Mirror.Singleton =>
                  Attempt.successful(DecodeResult(s.fromProduct(EmptyProduct).asInstanceOf[S], b))
                case p: Mirror.ProductOf[`hd` & S] =>
                  val hdLabelValue = constValue[hdLabel].asInstanceOf[String]
                  inline val size = constValue[Tuple.Size[p.MirroredElemTypes]]
                  val elems = new ArrayProduct(size)
                  decodeElems[p.MirroredElemTypes, p.MirroredElemLabels](b, 0, elems).mapErr(_.pushContext(hdLabelValue)).map(_.map(_ => p.fromProduct(elems)))
              }
            } else decodeCases[S, tl, tlLabels](b, ordinal, i + 1)
          case _: Unit =>
            sys.error("not possible - label for product not available")
        }
      case _: Unit =>
        Attempt.failure(Err.MatchingDiscriminatorNotFound(ordinal, Nil))
    }

  private inline def summonOne[A]: A = summonFrom { case a: A => a }

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

  given [A](using ccount: Codec[Int], ca: Codec[A]) as Codec[List[A]] = codecs.listOfN(ccount, ca)
  given [A](using ccount: Codec[Int], ca: Codec[A]) as Codec[Vector[A]] = codecs.vectorOfN(ccount, ca)
  given [A](using cguard: Codec[Boolean], ca: Codec[A]) as Codec[Option[A]] = codecs.optional(cguard, ca)

  given Transform[Codec] {
    def [A, B](fa: Codec[A]).exmap(f: A => Attempt[B], g: B => Attempt[A]): Codec[B] = 
      fa.exmap(f, g)
  }
}
