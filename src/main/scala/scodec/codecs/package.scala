package scodec

import java.nio.charset.Charset
import java.security.cert.Certificate
import java.util.UUID

import shapeless.Iso
import scodec.bits.{ BitVector, ByteVector }

/**
 * Provides codecs for common types and combinators for building larger codecs.
 *
 * === Basic Codecs ===
 *
 * There are built-in codecs for `Int`, `Long`, `Boolean`, `String`, and `UUID`.
 *
 * There are a number of predefined integral codecs named using the form: {{{
 [u]int${size}[L]
 }}}
 * where `u` stands for unsigned, `size` is replaced by one of `8, 16, 24, 32, 64`, and `L` stands for little-endian.
 * For each codec of that form, the type is `Codec[Int]` or `Codec[Long]` depending on the specified size.
 * For example, `int32` supports 32-bit big-endian 2s complement signed integers, and uint16L supports 16-bit little-endian
 * unsigned integers.
 * Note: `uint64[L]` is not provided because it does not fit in to a `Long`.
 *
 * Additionally, methods of the form `[u]int[L](size: Int)` and `[u]long[L](size: Int)` exist to build arbitrarily
 * sized codecs, within the limitations of `Int` and `Long`.
 *
 * === Tuple Codecs ===
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
 * Note: this design is heavily based on Scala's parser combinator library and the syntax it provides.
 */
package object codecs {

  val int8: Codec[Int] = new IntCodec(8)
  val int16: Codec[Int] = new IntCodec(16)
  val int24: Codec[Int] = new IntCodec(24)
  val int32: Codec[Int] = new IntCodec(32)
  val int64: Codec[Long] = new LongCodec(64)

  val uint2: Codec[Int] = new IntCodec(2, signed = false)
  val uint4: Codec[Int] = new IntCodec(4, signed = false)
  val uint8: Codec[Int] = new IntCodec(8, signed = false)
  val uint16: Codec[Int] = new IntCodec(16, signed = false)
  val uint24: Codec[Int] = new IntCodec(24, signed = false)
  val uint32: Codec[Long] = new LongCodec(32, signed = false)

  val int8L: Codec[Int] = new IntCodec(8, bigEndian = false)
  val int16L: Codec[Int] = new IntCodec(16, bigEndian = false)
  val int24L: Codec[Int] = new IntCodec(24, bigEndian = false)
  val int32L: Codec[Int] = new IntCodec(32, bigEndian = false)
  val int64L: Codec[Long] = new LongCodec(64, bigEndian = false)

  val uint2L: Codec[Int] = new IntCodec(2, signed = false, bigEndian = false)
  val uint4L: Codec[Int] = new IntCodec(4, signed = false, bigEndian = false)
  val uint8L: Codec[Int] = new IntCodec(8, signed = false, bigEndian = false)
  val uint16L: Codec[Int] = new IntCodec(16, signed = false, bigEndian = false)
  val uint24L: Codec[Int] = new IntCodec(24, signed = false, bigEndian = false)
  val uint32L: Codec[Long] = new LongCodec(32, signed = false, bigEndian = false)

  def int(bits: Int): Codec[Int] = new IntCodec(bits)
  def uint(bits: Int): Codec[Int] = new IntCodec(bits, signed = false)
  def long(bits: Int): Codec[Long] = new LongCodec(bits)
  def ulong(bits: Int): Codec[Long] = new LongCodec(bits, signed = false)

  def intL(bits: Int): Codec[Int] = new IntCodec(bits, bigEndian = false)
  def uintL(bits: Int): Codec[Int] = new IntCodec(bits, signed = false, bigEndian = false)
  def longL(bits: Int): Codec[Long] = new LongCodec(bits, bigEndian = false)
  def ulongL(bits: Int): Codec[Long] = new LongCodec(bits, signed = false, bigEndian = false)

  val bool: Codec[Boolean] = BooleanCodec

  def string(implicit charset: Charset): Codec[String] = new StringCodec(charset)
  val ascii = string(Charset.forName("US-ASCII"))
  val utf8 = string(Charset.forName("UTF-8"))

  val uuid: Codec[UUID] = UuidCodec

  def provide[A](value: A): Codec[A] = new ProvideCodec(value)

  def ignore(bits: Int): Codec[Unit] = new IgnoreCodec(bits)

  def constant(bits: BitVector): Codec[Unit] = new ConstantCodec(bits)
  def constant[A: Integral](bits: A*): Codec[Unit] = new ConstantCodec(BitVector(bits: _*))

  def fixedSizeBits[A](size: Int, codec: Codec[A]): Codec[A] = new FixedSizeCodec(size, codec)
  def fixedSizeBytes[A](size: Int, codec: Codec[A]): Codec[A] = fixedSizeBits(size * 8, codec).withToString(s"fixedSizeBytes($size, $codec)")

  def variableSizeBits[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    new VariableSizeCodec(size, value, sizePadding)
  def variableSizeBytes[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    variableSizeBits(size.xmap[Int](_ * 8, _ / 8).withToString(size.toString), value, sizePadding * 8).withToString(s"variableSizeBytes($size, $value)")

  def bits: Codec[BitVector] = BitVectorCodec.withToString("bits")
  def bits(size: Int): Codec[BitVector] = fixedSizeBits(size, BitVectorCodec).withToString(s"bits($size)")

  def bytes: Codec[ByteVector] = BitVectorCodec.xmap[ByteVector](_.toByteVector, _.toBitVector).withToString(s"bytes")
  def bytes(size: Int): Codec[ByteVector] = fixedSizeBytes(size, BitVectorCodec).xmap[ByteVector](_.toByteVector, _.toBitVector).withToString(s"bytes($size)")

  def conditional[A](included: Boolean, codec: Codec[A]): Codec[Option[A]] = new ConditionalCodec(included, codec)

  def repeated[A](codec: Codec[A]): Codec[collection.immutable.IndexedSeq[A]] = new IndexedSeqCodec(codec)

  def encrypted[A](codec: Codec[A])(implicit cipherFactory: CipherFactory): Codec[A] = new CipherCodec(codec)(cipherFactory)

  def fixedSizeSignature[A](byteSize: Int)(codec: Codec[A])(implicit signatureFactory: SignatureFactory): Codec[A] =
    new SignatureCodec(codec, fixedSizeBytes(byteSize, BitVectorCodec))(signatureFactory)

  def variableSizeSignature[A](byteSizeCodec: Codec[Int])(codec: Codec[A])(implicit signatureFactory: SignatureFactory): Codec[A] =
    new SignatureCodec(codec, variableSizeBytes(byteSizeCodec, BitVectorCodec))(signatureFactory)

  val x509Certificate: Codec[Certificate] = new CertificateCodec("X.509")

  /**
   * Provides the `|` method on `String` that allows creation of a named codec.
   *
   * Usage: {{{val codec = "id" | uint8}}}
   */
  final implicit class StringEnrichedWithCodecNamingSupport(val name: String) extends AnyVal {
    /** Names the specified codec, resulting in the name being included in error messages. */
    def |[A](codec: Codec[A]): Codec[A] = new NamedCodec(name, codec)
  }

  /** Builds an `Iso[A, B]` from two functions. */
  final def isoFromFunctions[A, B](to: A => B, from: B => A): Iso[A, B] = {
    val toFn = to
    val fromFn = from
    new Iso[A, B] {
      def to(a: A) = toFn(a)
      def from(b: B) = fromFn(b)
    }
  }

  // Tuple codec syntax

  /** Type alias for Tuple2 in order to allow left nested tuples to be written as A ~ B ~ C ~ .... */
  final type ~[+A, +B] = (A, B)

  /** Extractor that allows pattern matching on the tuples created by tupling codecs. */
  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = Some(t)
  }

  /** Allows creation of left nested tuples by successive usage of `~` operator. */
  final implicit class ValueEnrichedWithTuplingSupport[A](val a: A) {
    def ~[B](b: B): (A, B) = (a, b)
  }

  final implicit def liftF2ToNestedTupleF[A, B, X](fn: (A, B) => X): ((A, B)) => X =
    fn.tupled
  final implicit def liftF3ToNestedTupleF[A, B, C, X](fn: (A, B, C) => X): (((A, B), C)) => X = {
    case a ~ b ~ c => fn(a, b, c)
  }
  final implicit def liftF4ToNestedTupleF[A, B, C, D, X](fn: (A, B, C, D) => X): ((((A, B), C), D)) => X = {
    case a ~ b ~ c ~ d => fn(a, b, c, d)
  }
  final implicit def liftF5ToNestedTupleF[A, B, C, D, E, X](fn: (A, B, C, D, E) => X): (((((A, B), C), D), E)) => X = {
    case a ~ b ~ c ~ d ~ e => fn(a, b, c, d, e)
  }
  final implicit def liftF6ToNestedTupleF[A, B, C, D, E, F, X](fn: (A, B, C, D, E, F) => X): ((((((A, B), C), D), E), F)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f => fn(a, b, c, d, e, f)
  }
  final implicit def liftF7ToNestedTupleF[A, B, C, D, E, F, G, X](fn: (A, B, C, D, E, F, G) => X): (((((((A, B), C), D), E), F), G)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f ~ g => fn(a, b, c, d, e, f, g)
  }
  final implicit def liftF8ToNestedTupleF[A, B, C, D, E, F, G, H, X](fn: (A, B, C, D, E, F, G, H) => X): ((((((((A, B), C), D), E), F), G), H)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h => fn(a, b, c, d, e, f, g, h)
  }

  // DiscriminatorCodec syntax

  import DiscriminatorCodecSyntax._

  /**
   * Provides syntax for building a [[DiscriminatorCodec]].
   * Usage: {{{
   val discriminator: Discriminator[AnyVal, Int] = ???
   val codec = discriminated[AnyVal] by uint8 using discriminator
   }}}
   */
  final def discriminated[A]: NeedDiscriminatorCodec[A] = DiscriminatorCodecSyntax.discriminated[A]

  final def typeDiscriminator[A, B](cases: TypeDiscriminatorCase[_ <: A, B]*): Discriminator[A, B] =
    new TypeDiscriminator[A, B](cases.toIndexedSeq)

  final def typeDiscriminatorCase[A: Manifest, B](discriminatorValue: B, codec: Codec[A]): TypeDiscriminatorCase[A, B] =
    TypeDiscriminatorCase(discriminatorValue, codec)
}

