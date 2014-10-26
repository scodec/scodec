package scodec

import scala.language.implicitConversions

import java.nio.charset.Charset
import java.security.cert.{ Certificate, X509Certificate }
import java.util.UUID

import scalaz.{ \/, -\/, \/- }
import scalaz.syntax.std.option._
import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

/**
 * Provides codecs for common types and combinators for building larger codecs.
 *
 * === Bits and Bytes Codecs ===
 *
 * The simplest of the provided codecs are those that encode/decode `BitVector`s and `ByteVectors` directly.
 * These are provided by [[bits]] and [[bytes]] methods. These codecs encode all of the bits/bytes directly
 * in to the result and decode *all* of the remaining bits/bytes in to the result value. That is, the result
 * of `decode` always returns a empty bit vector for the remaining bits.
 *
 * Similarly, fixed size alternatives are provided by the `bits(size)` and `bytes(size)` methods, which
 * encode a fixed number of bits/bytes (or error if not provided the correct size) and decoded a fixed number
 * of bits/bytes (or error if that many bits/bytes are not available).
 *
 * There are more specialized codecs for working with bits, including [[ignore]] and [[constant]].
 *
 *
 * === Numeric Codecs ===
 *
 * There are built-in codecs for `Int`, `Long`, `Float`, and `Double`.
 *
 * There are a number of predefined integral codecs named using the form: {{{
 [u]int${size}[L]
 }}}
 * where `u` stands for unsigned, `size` is replaced by one of `8, 16, 24, 32, 64`, and `L` stands for little-endian.
 * For each codec of that form, the type is `Codec[Int]` or `Codec[Long]` depending on the specified size.
 * For example, `int32` supports 32-bit big-endian 2s complement signed integers, and uint16L supports 16-bit little-endian
 * unsigned integers.
 * Note: `uint64[L]` are not provided because a 64-bit unsigned integer does not fit in to a `Long`.
 *
 * Additionally, methods of the form `[u]int[L](size: Int)` and `[u]long[L](size: Int)` exist to build arbitrarily
 * sized codecs, within the limitations of `Int` and `Long`.
 *
 * IEEE 754 floating point values are supported by the [[float]], [[floatL]], [[double]], and [[doubleL]] codecs.
 *
 *
 * === Miscellaneous Value Codecs ===
 *
 * In addition to the numeric codecs, there are built-in codecs for `Boolean`, `String`, and `UUID`.
 *
 * Boolean values are supported by the [[bool]] codecs.
 *
 *
 * === Combinators ===
 *
 * There are a number of methods provided that create codecs out of other codecs. These include simple combinators
 * such as [[fixedSizeBits]] and [[variableSizeBits]] and advanced combinators such as [[discriminated]], which
 * provides its own DSL for building a large codec out of many small codecs. For a list of all combinators,
 * see the Combinators section below.
 *
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
 *
 *
 * === Cryptograhpy Codecs ===
 *
 * There are codecs that support working with encrypted data ([[encrypted]]) and digital signatures
 * ([[fixedSizeSignature]] and [[variableSizeSignature]]). Additionally, support for `java.security.cert.Certificate`s
 * is provided by [[certificate]] and [[x509Certificate]].
 *
 *
 * @groupname bits Bits and Bytes Codecs
 * @groupprio bits 0
 *
 * @groupname numbers Number Codecs
 * @groupprio numbers 1
 *
 * @groupname values Miscellaneous Value Codecs
 * @groupprio values 2
 *
 * @groupname combinators Combinators
 * @groupprio combinators 3
 *
 * @groupname tuples Tuple Support
 * @groupprio tuples 3
 *
 * @groupname crypto Cryptography
 * @groupprio crypto 4
 */
package object codecs {

  /**
   * Encodes by returning supplied bit vector; decodes by taking all remaining bits in the supplied bit vector.
   * @group bits
   */
  def bits: Codec[BitVector] = BitVectorCodec.withToString("bits")

  /**
   * Encodes by returning the supplied bit vector if its length is `size` bits, otherwise returning error;
   * decodes by taking `size` bits from the supplied bit vector.
   *
   * @param size number of bits to encode/decode
   * @group bits
   */
  def bits(size: Long): Codec[BitVector] = new Codec[BitVector] {
    private val codec = fixedSizeBits(size, BitVectorCodec)
    def encode(b: BitVector) = codec.encode(b)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"bits($size)"
  }

  /**
   * Encodes by returning supplied byte vector as a bit vector; decodes by taking all remaining bits in supplied bit vector and converting to a byte vector.
   * @group bits
   */
  def bytes: Codec[ByteVector] = bits.xmap[ByteVector](_.toByteVector, _.toBitVector).withToString("bytes")

  /**
   * Encodes by returning the supplied byte vector if its length is `size` bytes, otherwise returning error;
   * decodes by taking `size * 8` bits from the supplied bit vector and converting to a byte vector.
   *
   * @param size number of bits to encode/decode
   * @group bits
   */
  def bytes(size: Int): Codec[ByteVector] = new Codec[ByteVector] {
    private val codec = fixedSizeBytes(size, BitVectorCodec).xmap[ByteVector](_.toByteVector, _.toBitVector)
    def encode(b: ByteVector) = codec.encode(b)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"bytes($size)"
  }

  /**
   * Codec for 8-bit 2s complement bytes.
   * @group numbers
   */
  val byte: Codec[Byte] = new ByteCodec(8, true)

  /**
   * Codec for 8-bit unsigned bytes.
   * @group numbers
   */
  val ushort8: Codec[Short] = new ShortCodec(8, true, ByteOrdering.BigEndian)

  /**
   * Codec for 16-bit 2s complement big-endian shorts.
   * @group numbers
   */
  val short16: Codec[Short] = new ShortCodec(16, true, ByteOrdering.BigEndian)

  /**
   * Codec for 8-bit 2s complement big-endian integers.
   * @group numbers
   */
  val int8: Codec[Int] = new IntCodec(8, true, ByteOrdering.BigEndian)

  /**
   * Codec for 16-bit 2s complement big-endian integers.
   * @group numbers
   */
  val int16: Codec[Int] = new IntCodec(16, true, ByteOrdering.BigEndian)

  /**
   * Codec for 24-bit 2s complement big-endian integers.
   * @group numbers
   */
  val int24: Codec[Int] = new IntCodec(24, true, ByteOrdering.BigEndian)

  /**
   * Codec for 32-bit 2s complement big-endian integers.
   * @group numbers
   */
  val int32: Codec[Int] = new IntCodec(32, true, ByteOrdering.BigEndian)

  /**
   * Codec for 64-bit 2s complement big-endian integers.
   * @group numbers
   */
  val int64: Codec[Long] = new LongCodec(64, true, ByteOrdering.BigEndian)

  /**
   * Codec for 2-bit unsigned big-endian integers.
   * @group numbers
   */
  val uint2: Codec[Int] = new IntCodec(2, false, ByteOrdering.BigEndian)

  /**
   * Codec for 4-bit unsigned big-endian integers.
   * @group numbers
   */
  val uint4: Codec[Int] = new IntCodec(4, false, ByteOrdering.BigEndian)

  /**
   * Codec for 8-bit unsigned big-endian integers.
   * @group numbers
   */
  val uint8: Codec[Int] = new IntCodec(8, false, ByteOrdering.BigEndian)

  /**
   * Codec for 16-bit unsigned big-endian integers.
   * @group numbers
   */
  val uint16: Codec[Int] = new IntCodec(16, false, ByteOrdering.BigEndian)

  /**
   * Codec for 24-bit unsigned big-endian integers.
   * @group numbers
   */
  val uint24: Codec[Int] = new IntCodec(24, false, ByteOrdering.BigEndian)

  /**
   * Codec for 32-bit unsigned big-endian integers.
   * @group numbers
   */
  val uint32: Codec[Long] = new LongCodec(32, false, ByteOrdering.BigEndian)

  /**
   * Codec for 16-bit 2s complement little-endian shorts.
   * @group numbers
   */
  val short16L: Codec[Short] = new ShortCodec(16, true, ByteOrdering.LittleEndian)

  /**
   * Codec for 8-bit 2s complement little-endian integers.
   * @group numbers
   */
  val int8L: Codec[Int] = new IntCodec(8, true, ByteOrdering.LittleEndian)

  /**
   * Codec for 16-bit 2s complement little-endian integers.
   * @group numbers
   */
  val int16L: Codec[Int] = new IntCodec(16, true, ByteOrdering.LittleEndian)

  /**
   * Codec for 24-bit 2s complement little-endian integers.
   * @group numbers
   */
  val int24L: Codec[Int] = new IntCodec(24, true, ByteOrdering.LittleEndian)

  /**
   * Codec for 32-bit 2s complement little-endian integers.
   * @group numbers
   */
  val int32L: Codec[Int] = new IntCodec(32, true, ByteOrdering.LittleEndian)

  /**
   * Codec for 64-bit 2s complement little-endian integers.
   * @group numbers
   */
  val int64L: Codec[Long] = new LongCodec(64, true, ByteOrdering.LittleEndian)

  /**
   * Codec for 2-bit unsigned little-endian integers.
   * @group numbers
   */
  val uint2L: Codec[Int] = new IntCodec(2, false, ByteOrdering.LittleEndian)

  /**
   * Codec for 4-bit unsigned little-endian integers.
   * @group numbers
   */
  val uint4L: Codec[Int] = new IntCodec(4, false, ByteOrdering.LittleEndian)

  /**
   * Codec for 8-bit unsigned little-endian integers.
   * @group numbers
   */
  val uint8L: Codec[Int] = new IntCodec(8, false, ByteOrdering.LittleEndian)

  /**
   * Codec for 16-bit unsigned little-endian integers.
   * @group numbers
   */
  val uint16L: Codec[Int] = new IntCodec(16, false, ByteOrdering.LittleEndian)

  /**
   * Codec for 24-bit unsigned little-endian integers.
   * @group numbers
   */
  val uint24L: Codec[Int] = new IntCodec(24, false, ByteOrdering.LittleEndian)

  /**
   * Codec for 32-bit unsigned little-endian integers.
   * @group numbers
   */
  val uint32L: Codec[Long] = new LongCodec(32, false, ByteOrdering.LittleEndian)

  /**
   * Codec for n-bit 2s complement bytes.
   * @param size number of bits (must be 0 < size <= 8)
   * @group numbers
   */
  def byte(size: Int): Codec[Byte] = new ByteCodec(size, true)

  /**
   * Codec for n-bit unsigned bytes.
   * @param size number of bits (must be 0 < size <= 7)
   * @group numbers
   */
  def ubyte(size: Int): Codec[Byte] = new ByteCodec(size, false)

  /**
   * Codec for n-bit 2s complement big-endian shorts.
   * @param size number of bits (must be 0 < size <= 16)
   * @group numbers
   */
  def short(size: Int): Codec[Short] = new ShortCodec(size, true, ByteOrdering.BigEndian)

  /**
   * Codec for n-bit unsigned big-endian shorts.
   * @param size number of bits (must be 0 < size <= 15)
   * @group numbers
   */
  def ushort(size: Int): Codec[Short] = new ShortCodec(size, false, ByteOrdering.BigEndian)

  /**
   * Codec for n-bit 2s complement big-endian integers that are represented with `Int`.
   * @param size number of bits (must be 0 < size <= 32)
   * @group numbers
   */
  def int(size: Int): Codec[Int] = new IntCodec(size, true, ByteOrdering.BigEndian)

  /**
   * Codec for n-bit unsigned big-endian integers that are represented with `Int`.
   * @param bits number of bits (must be 0 < size <= 31)
   * @group numbers
   */
  def uint(bits: Int): Codec[Int] = new IntCodec(bits, false, ByteOrdering.BigEndian)

  /**
   * Codec for n-bit 2s complement big-endian integers that are represented with `Long`.
   * @param bits number of bits (must be 0 < size <= 64)
   * @group numbers
   */
  def long(bits: Int): Codec[Long] = new LongCodec(bits, true, ByteOrdering.BigEndian)

  /**
   * Codec for n-bit unsigned big-endian integers that are represented with `Long`.
   * @param bits number of bits (must be 0 < size <= 63)
   * @group numbers
   */
  def ulong(bits: Int): Codec[Long] = new LongCodec(bits, false, ByteOrdering.BigEndian)

  /**
   * Codec for n-bit 2s complement little-endian shorts.
   * @param size number of bits (must be 0 < size <= 16)
   * @group numbers
   */
  def shortL(size: Int): Codec[Short] = new ShortCodec(size, true, ByteOrdering.LittleEndian)

  /**
   * Codec for n-bit unsigned little-endian shorts.
   * @param size number of bits (must be 0 < size <= 15)
   * @group numbers
   */
  def ushortL(size: Int): Codec[Short] = new ShortCodec(size, false, ByteOrdering.LittleEndian)

  /**
   * Codec for n-bit 2s complement little-endian integers that are represented with `Int`.
   * @param bits number of bits (must be 0 < size <= 32)
   * @group numbers
   */
  def intL(bits: Int): Codec[Int] = new IntCodec(bits, true, ByteOrdering.LittleEndian)

  /**
   * Codec for n-bit unsigned little-endian integers that are represented with `Int`.
   * @param bits number of bits (must be 0 < size <= 31)
   * @group numbers
   */
  def uintL(bits: Int): Codec[Int] = new IntCodec(bits, false, ByteOrdering.LittleEndian)

  /**
   * Codec for n-bit 2s complement little-endian integers that are represented with `Long`.
   * @param bits number of bits (must be 0 < size <= 64)
   * @group numbers
   */
  def longL(bits: Int): Codec[Long] = new LongCodec(bits, true, ByteOrdering.LittleEndian)

  /**
   * Codec for n-bit unsigned little-endian integers that are represented with `Long`.
   * @param bits number of bits (must be 0 < size <= 63)
   * @group numbers
   */
  def ulongL(bits: Int): Codec[Long] = new LongCodec(bits, false, ByteOrdering.LittleEndian)

  /**
   * 32-bit big endian IEEE 754 floating point number.
   * @group numbers
   */
  val float: Codec[Float] = new FloatCodec(ByteOrdering.BigEndian)

  /**
   * 32-bit little endian IEEE 754 floating point number.
   * @group numbers
   */
  val floatL: Codec[Float] = new FloatCodec(ByteOrdering.LittleEndian)

  /**
   * 64-bit big endian IEEE 754 floating point number.
   * @group numbers
   */
  val double: Codec[Double] = new DoubleCodec(ByteOrdering.BigEndian)

  /**
   * 64-bit little endian IEEE 754 floating point number.
   * @group numbers
   */
  val doubleL: Codec[Double] = new DoubleCodec(ByteOrdering.LittleEndian)

  /**
   * 1-bit boolean codec, where false corresponds to bit value 0 and true corresponds to bit value 1.
   * @group values
   */
  val bool: Codec[Boolean] = BooleanCodec

  /**
   * n-bit boolean codec, where false corresponds to bit vector of all 0s and true corresponds to all other vectors.
   * @group values
   */
  def bool(n: Long): Codec[Boolean] = new Codec[Boolean] {
    private val zeros = BitVector.low(n)
    private val ones = BitVector.high(n)
    private val codec = bits(n).xmap[Boolean](bits => !(bits == zeros), b => if (b) ones else zeros)
    def encode(b: Boolean) = codec.encode(b)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = "bool($n)"
  }

  /**
   * String codec that utilizes the implicit `Charset` to perform encoding/decoding.
   *
   * This codec does not encode the size of the string in to the output. Hence, decoding
   * a vector that has additional data after the encoded string will result in
   * unexpected output. Instead, it is common to use this codec along with either
   * [[fixedSizeBits]] or [[variableSizeBits]]. For example, a common encoding
   * is a size field, say 2 bytes, followed by the encoded string. This can be
   * accomplished with: {{{variableSizeBits(uint8, string)}}}
   *
   * @param charset charset to use to convert strings to/from binary
   * @group values
   */
  def string(implicit charset: Charset): Codec[String] = new StringCodec(charset)

  /**
   * String codec that uses the `US-ASCII` charset. See [[string]] for more information on `String` codecs.
   * @group values
   */
  val ascii = string(Charset.forName("US-ASCII"))

  /**
   * String codec that uses the `US-ASCII` charset. See [[string]] for more information on `String` codecs.
   * @group values
   */
  val utf8 = string(Charset.forName("UTF-8"))

  /**
   * Encodes/decodes `UUID`s as 2 64-bit big-endian longs, first the high 64-bits then the low 64-bits.
   * @group values
   */
  val uuid: Codec[UUID] = UuidCodec

  /**
   * Codec that always returns an empty vector from `encode` and always returns `(empty, value)` from `decode`.
   * This is often useful when combined with other codecs (e.g., the [[discriminated]]).
   * @param value value to return from decode
   * @group combinators
   */
  def provide[A](value: A): Codec[A] = new ProvideCodec(value)

  /**
   * Codec that always encodes `size` 0 bits and always decodes `size` bits and then discards them, returning `()` instead.
   * @param size number of bits to ignore
   * @group bits
   */
  def ignore(size: Long): Codec[Unit] = new IgnoreCodec(size)

  /**
   * Codec that always encodes the specified bits and always decodes the specified bits, returning `()` if the actual bits match
   * the specified bits and returning an error otherwise.
   * @param bits constant bits
   * @group bits
   */
  def constant(bits: BitVector): Codec[Unit] = new ConstantCodec(bits)

  /**
   * Codec that always encodes the specified bytes and always decodes the specified bytes, returning `()` if the actual bytes match
   * the specified bytes and returning an error otherwise.
   * @param bytes constant bytes
   * @group bits
   */
  def constant(bytes: ByteVector): Codec[Unit] = constant(bytes.bits)

  /**
   * Codec that always encodes the specified bits and always decodes the specified bits, returning `()` if the actual bits match
   * the specified bits and returning an error otherwise.
   * @param bits constant bits
   * @group bits
   */
  def constant[A: Integral](bits: A*): Codec[Unit] = constant(BitVector(bits: _*))

  /**
   * Codec that always encodes the specified bits and always decodes n bits, returning `()`, where n is the length of the
   * specified bits.
   * @param bits constant bits
   * @group bits
   */
  def constantLenient(bits: BitVector): Codec[Unit] = new ConstantCodec(bits, false)

  /**
   * Codec that always encodes the specified bytes and always decodes n bytes, returning `()`, where n is the length of the
   * specified bytes.
   * @param bytes constant bytes
   * @group bits
   */
  def constantLenient(bytes: ByteVector): Codec[Unit] = constantLenient(bytes.bits)

  /**
   * Codec that always encodes the specified bits and always decodes n bits, returning `()`, where n is the length of the
   * specified bits.
   * @param bits constant bits
   * @group bits
   */
  def constantLenient[A: Integral](bits: A*): Codec[Unit] = constantLenient(BitVector(bits: _*))

  /**
   * Provides implicit conversions from literal types to constant codecs.
   *
   * For example, with `literals._` imported, `constant(0x47) ~> uint8`
   * can be written as `0x47 ~> uint8`.
   *
   * Supports literal bytes, ints, `BitVector`s, and `ByteVector`s.
   *
   * @group bits
   */
  object literals {
    implicit def constantIntCodec(a: Int): Codec[Unit] = constant(a)
    implicit def constantByteVectorCodec(a: ByteVector): Codec[Unit] = constant(a)
    implicit def constantBitVectorCodec(a: BitVector): Codec[Unit] = constant(a)
  }

  /**
   * Codec that limits the number of bits the specified codec works with.
   *
   * When encoding, if encoding with the specified codec
   * results in less than the specified size, the vector is right padded with 0 bits. If the result is larger than the specified
   * size, an encoding error is returned.
   *
   * When decoding, the specified codec is only given `size` bits. If the specified codec does not consume all the bits it was
   * given, any remaining bits are discarded.
   *
   * @param size number of bits
   * @param codec codec to limit
   * @group combinators
   */
  def fixedSizeBits[A](size: Long, codec: Codec[A]): Codec[A] = new FixedSizeCodec(size, codec)

  /**
   * Byte equivalent of [[fixedSizeBits]].
   * @param size number of bytes
   * @param codec codec to limit
   * @group combinators
   */
  def fixedSizeBytes[A](size: Long, codec: Codec[A]): Codec[A] = new Codec[A] {
    private val fcodec = fixedSizeBits(size * 8, codec)
    def encode(a: A) = fcodec.encode(a)
    def decode(b: BitVector) = fcodec.decode(b)
    override def toString = s"fixedSizeBytes($size, $codec)"
  }

  /**
   * Codec that limits the number of bits the specified codec works with.
   *
   * If the encoded result is larger than the specified
   * size, an encoding error is returned.
   *
   * If encoding with the specified codec
   * results in less than the specified size, the vector is right padded by repeatedly encoding with padCodec.
   * An encoding error is returned if the padCodec result does not precisely fill the remaining space.
   *
   * When decoding, the specified codec is only given `size` bits. If the specified codec does not consume all the bits it was
   * given, all remaining bits are repeatedly decoded by padCodec. A decoding error is returned if any
   * padCodec decode returns an error.
   *
   * @param size number of bits
   * @param codec codec to limit
   * @param padCodec codec to handle excess space
   * @group combinators
   */
   def paddedFixedSizeBits[A](size: Long, codec: Codec[A], padCodec:Codec[Unit]): Codec[A] = new PaddedFixedSizeCodec(size, codec, padCodec)

   /**
   * Byte equivalent of [[paddedFixedSizeBits]].
   * @param size number of bytes
   * @param codec codec to limit
   * @param padCodec codec to handle excess space
   * @group combinators
   */
   def paddedFixedSizeBytes[A](size: Long, codec: Codec[A], padCodec:Codec[Unit]): Codec[A] = new Codec[A] {
    private val fcodec = paddedFixedSizeBits(size * 8, codec, padCodec)
    def encode(a: A) = fcodec.encode(a)
    def decode(b: BitVector) = fcodec.decode(b)
    override def toString = s"paddedFixedSizeBytes($size, $codec, $padCodec)"
  }

  /**
   * Codec that supports vectors of the form `size ++ value` where the `size` field decodes to the bit length of the `value` field.
   *
   * For example, encoding the string `"hello"` with `variableSizeBits(uint8, ascii)` yields a vector of 6 bytes -- the first byte being
   * 0x05 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * The `size` field can be any `Int` codec. An optional padding can be applied to the size field. The `sizePadding` is added to
   * the calculated size before encoding, and subtracted from the decoded size before decoding the value.
   *
   * For example, encoding `"hello"` with `variableSizeBits(uint8, ascii, 1)` yields a vector of 6 bytes -- the first byte being
   * 0x06 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bits to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizeBits[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    variableSizeBitsLong(widenIntToLong(size), value, sizePadding)

  /**
   * Byte equivalent of [[variableSizeBits]].
   * @param size codec that encodes/decodes the size in bytes
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bytes to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizeBytes[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    variableSizeBytesLong(widenIntToLong(size), value, sizePadding)

  private def widenIntToLong(c: Codec[Int]): Codec[Long] =
    c.widen(i => i, l => if (l > Int.MaxValue && l < Int.MinValue) \/.left(s"$l cannot be converted to an integer") else \/.right(l.toInt))

  /**
   * Codec that supports vectors of the form `size ++ value` where the `size` field decodes to the bit length of the `value` field.
   *
   * For example, encoding the string `"hello"` with `variableSizeBitsLong(uint32, ascii)` yields a vector of 9 bytes -- the first four bytes being
   * 0x00000005 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * The `size` field can be any `Long` codec. An optional padding can be applied to the size field. The `sizePadding` is added to
   * the calculated size before encoding, and subtracted from the decoded size before decoding the value.
   *
   * For example, encoding `"hello"` with `variableSizeBitsLong(uint32, ascii, 1)` yields a vector of 9 bytes -- the first 4 bytes being
   * 0x00000006 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bits to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizeBitsLong[A](size: Codec[Long], value: Codec[A], sizePadding: Long = 0): Codec[A] =
    new VariableSizeCodec(size, value, sizePadding)

  /**
   * Byte equivalent of [[variableSizeBitsLong]].
   * @param size codec that encodes/decodes the size in bytes
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bytes to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizeBytesLong[A](size: Codec[Long], value: Codec[A], sizePadding: Long = 0): Codec[A] = new Codec[A] {
    private val codec = variableSizeBitsLong(size.xmap[Long](_ * 8, _ / 8), value, sizePadding * 8)
    def encode(a: A) = codec.encode(a)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"variableSizeBytes($size, $value)"
  }

  /**
   * Codec of `Option[A]` that delegates to a `Codec[A]` when the `included` parameter is true.
   *
   * When encoding, if `included` is true and the value to encode is a `Some`, the specified codec is used to encode the inner value.
   * Otherwise, an empty bit vector is returned.
   *
   * When decoding, if `included` is true, the specified codec is used and its result is wrapped in a `Some`. Otherwise, a `None` is returned.
   *
   * @param included whether this codec is enabled (meaning it delegates to the specified codec) or disabled, in which case it
   * encodes no bits and returns `None` from decode
   * @param codec codec to conditionally include
   * @group combinators
   */
  def conditional[A](included: Boolean, codec: Codec[A]): Codec[Option[A]] = new ConditionalCodec(included, codec)

  /**
   * Codec of `Option[A]` that delegates to a `Codec[A]` when the `guard` codec decodes a true.
   *
   * When encoding, a `Some` results in `guard` encoding a `true` and `target` encoding the value.
   * A `None` results in `guard` encoding a false and the `target` not encoding anything.
   *
   * @param guard codec that determines whether the target codec is included
   * @param target codec to conditionally include
   * @group combinators
   */
  def optional[A](guard: Codec[Boolean], target: Codec[A]): Codec[Option[A]] =
    either(guard, provide(()), target).
      xmap[Option[A]](_.toOption, _.toRightDisjunction(())).
      withToString(s"optional($guard, $target)")

  /**
   * Creates a `Codec[A]` from a `Codec[Option[A]]` and a fallback `Codec[A]`.
   *
   * When encoding, the `A` is encoded with `opt` (by wrapping it in a `Some`).
   * When decoding, `opt` is first used to decode the buffer. If it decodes a `Some(a)`, that
   * value is returned. If it decodes a `None`, `default` is used to decode the buffer.
   *
   * @param opt optional codec
   * @param default fallback codec used during decoding when `opt` decodes a `None`
   * @group combinators
   */
  def withDefault[A](opt: Codec[Option[A]], default: Codec[A]): Codec[A] = {
    val paired = opt flatZip {
      case Some(a) => provide(a)
      case None => default
    }
    paired.xmap[A](_._2, a => (Some(a), a)).withToString(s"withDefault($opt, $default)")
  }

  /**
   * Creates a `Codec[A]` from a `Codec[Option[A]]` and a fallback value `A`.
   *
   * When encoding, the `A` is encoded with `opt` (by wrapping it in a `Some`).
   * When decoding, `opt` is first used to decode the buffer. If it decodes a `Some(a)`, that
   * value is returned. If it decodes a `None`, the `default` value is return.
   *
   * @param opt optional codec
   * @param default fallback value returned from `decode` when `opt` decodes a `None`
   * @group combinators
   */
  def withDefaultValue[A](opt: Codec[Option[A]], default: A): Codec[A] =
    withDefault(opt, provide(default))

  /**
   * Creates a codec that decodes true when the target codec decodes successfully and decodes false
   * when the target codec decodes unsuccessfully. Upon a successful decode of the target codec, the
   * remaining bits are returned, whereas upon an unsuccessful decode, the original input buffer is
   * returned.
   *
   * When encoding, a true results in the target codec encoding a unit whereas a false results
   * in encoding of an empty vector.
   *
   * @param target codec to recover errors from
   * @group combinators
   */
  def recover(target: Codec[Unit]): Codec[Boolean] = new RecoverCodec(target, false)

  /**
   * Lookahead version of [[recover]] -- i.e., upon successful decoding with the target codec,
   * the original buffer is returned instead of the remaining buffer.
   *
   * @param target codec to recover errors from
   * @group combinators
   */
  def lookahead(target: Codec[Unit]): Codec[Boolean] = new RecoverCodec(target, true)

  /**
   * Codec that encodes/decodes using the specified codecs by trying each codec in succession
   * and using the first successful result.
   *
   * @group combinators
   */
  def choice[A](codecs: Codec[A]*): Codec[A] =
    Codec(
      Encoder.choiceEncoder(codecs: _*),
      Decoder.choiceDecoder(codecs: _*)
    ).withToString(codecs.mkString("choice(", ", ", ")"))

  /**
   * Codec that encodes/decodes an immutable `IndexedSeq[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the sequence is encoded and all of the resulting vectors are concatenated.
   *
   * When decoding, `codec.decode` is called repeatedly until there are no more remaining bits and the value result
   * of each `decode` is returned in the sequence.
   *
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  @deprecated("Use vector codec or list codec instead.", "1.2.1")
  def repeated[A](codec: Codec[A]): Codec[collection.immutable.IndexedSeq[A]] =
    new VectorCodec(codec).xmap[collection.immutable.IndexedSeq[A]](identity, _.toVector)

  /**
   * Codec that encodes/decodes a `Vector[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the vector is encoded and all of the resulting vectors are concatenated.
   *
   * When decoding, `codec.decode` is called repeatedly until there are no more remaining bits and the value result
   * of each `decode` is returned in the vector.
   *
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  def vector[A](codec: Codec[A]): Codec[Vector[A]] = new VectorCodec(codec)

  /**
   * Codec that encodes/decodes a `Vector[A]` of `N` elements using a `Codec[A]`.
   *
   * When encoding, the number of elements in the vector is encoded using `countCodec`
   * and the values are then each encoded using `valueCodec`.
   *
   * When decoding, the number of elements is decoded using `countCodec` and then that number of elements
   * are decoded using `valueCodec`. Any remaining bits are returned.
   *
   * Note: when the count is known statically, use `vectorOfN(provide(count), ...)`.
   *
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  def vectorOfN[A](countCodec: Codec[Int], valueCodec: Codec[A]): Codec[Vector[A]] =
    countCodec.
      flatZip { count => new VectorCodec(valueCodec, Some(count)) }.
      xmap[Vector[A]]({ case (cnt, vec) => vec }, vec => (vec.size, vec)).
      withToString(s"vectorOfN($countCodec, $valueCodec)")

  /**
   * Codec that encodes/decodes a `List[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the list is encoded and all of the resulting vectors are concatenated.
   *
   * When decoding, `codec.decode` is called repeatedly until there are no more remaining bits and the value result
   * of each `decode` is returned in the list.
   *
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  def list[A](codec: Codec[A]): Codec[List[A]] = new ListCodec(codec)

  /**
   * Codec that encodes/decodes a `List[A]` of `N` elements using a `Codec[A]`.
   *
   * When encoding, the number of elements in the list is encoded using `countCodec`
   * and the values are then each encoded using `valueCodec`.
   *
   * When decoding, the number of elements is decoded using `countCodec` and then that number of elements
   * are decoded using `valueCodec`. Any remaining bits are returned.
   *
   * Note: when the count is known statically, use `listOfN(provide(count), ...)`.
   *
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  def listOfN[A](countCodec: Codec[Int], valueCodec: Codec[A]): Codec[List[A]] =
    countCodec.
      flatZip { count => new ListCodec(valueCodec, Some(count)) }.
      xmap[List[A]]({ case (cnt, xs) => xs }, xs => (xs.size, xs)).
      withToString(s"listOfN($countCodec, $valueCodec)")

  /**
   * Combinator that chooses amongst two codecs based on an implicitly available byte ordering.
   * @param big codec to use when big endian
   * @param little codec to use when little endian
   * @group combinators
   */
  def endiannessDependent[A](big: Codec[A], little: Codec[A])(implicit ordering: ByteOrdering): Codec[A] =
    ordering match {
      case ByteOrdering.BigEndian => big
      case ByteOrdering.LittleEndian => little
    }

  /**
   * Disjunction codec that supports vectors of form `indicator ++ (left or right)` where a
   * value of `false` for the indicator indicates it is followed by a left value and a value
   * of `true` indicates it is followed by a right value.
   * @param indicator codec that encodes/decodes false for left and true for right
   * @param left codec the encodes a left value
   * @param right codec the encodes a right value
   * @group combinators
   */
  def either[L, R](indicator: Codec[Boolean], left: Codec[L], right: Codec[R]): Codec[L \/ R] =
    discriminated[L \/ R].by(indicator)
    .| (false) { case -\/(l) => l } (\/.left) (left)
    .| (true)  { case \/-(r) => r } (\/.right) (right)

  /**
   * Like [[either]], but encodes the standard library `Either` type.
   * @param indicator codec that encodes/decodes false for left and true for right
   * @param left codec the encodes a left value
   * @param right codec the encodes a right value
   * @group combinators
   */
  def stdEither[L, R](indicator: Codec[Boolean], left: Codec[L], right: Codec[R]): Codec[Either[L,R]] =
    discriminated[Either[L,R]].by(indicator)
    .| (false) { case Left(l)  => l } (Left.apply) (left)
    .| (true)  { case Right(r) => r } (Right.apply) (right)


  /**
   * Provides a `Codec[A]` that delegates to a lazily evaluated `Codec[A]`.
   * @group combinators
   */
  def lazily[A](codec: => Codec[A]): Codec[A] = new Codec[A] {
    private lazy val c = codec
    def encode(a: A) = c.encode(a)
    def decode(b: BitVector) = c.decode(b)
    override def toString = s"lazily($c)"
  }

  /**
   * Codec that always fails encoding and decoding with the specified message.
   *
   * @group combinators
   */
  def fail[A](message: String): Codec[A] = fail(message, message)

  /**
   * Codec that always fails encoding and decoding with the specified messages.
   *
   * @group combinators
   */
  def fail[A](encMessage: String, decMessage: String): Codec[A] = new FailCodec[A](encMessage, decMessage)

  /**
   * Codec that encrypts and decrypts using a `javax.crypto.Cipher`.
   *
   * Encoding a value of type `A` is delegated to the specified codec and the resulting bit vector is encrypted
   * with a cipher provided by the implicit [[CipherFactory]].
   *
   * Decoding first decrypts all of the remaining bits and then decodes the decrypted bits with the
   * specified codec. Successful decoding always returns no remaining bits, even if the specified
   * codec does not consume all decrypted bits.
   *
   * @param codec codec that encodes a value to plaintext bits and decodes plaintext bits to a value
   * @param cipherFactory factory to use for encryption/decryption
   * @group crypto
   */
  def encrypted[A](codec: Codec[A])(implicit cipherFactory: CipherFactory): Codec[A] = new CipherCodec(codec)(cipherFactory)

  /**
   * Codec that includes a signature of the encoded bits.
   *
   * Encoding a value of type `A` is delegated to the specified codec and then a signature of those bits is
   * appended using the specified [[SignatureFactory]] to perform signing.
   *
   * Decoding first decodes using the specified codec and then all of the remaining bits are treated as
   * the signature of the decoded bits. The signature is verified and if it fails to verify, an error
   * is returned.
   *
   * Note: because decoding is first delegated to the specified code, care must be taken to ensure
   * that codec does not consume the signature bits. For example, if the target codec is an unbounded
   * string (e.g., ascii, utf8), decoding an encoded vector will result in the string codec trying to
   * decode the signature bits as part of the string.
   *
   * @param size size in bytes of signature
   * @param codec codec to use to encode/decode value field
   * @param signatureFactory factory to use for signing/verifying
   * @group crypto
   */
  def fixedSizeSignature[A](size: Int)(codec: Codec[A])(implicit signatureFactory: SignatureFactory): Codec[A] =
    new SignatureCodec(codec, fixedSizeBytes(size, BitVectorCodec))(signatureFactory)

  /**
   * Codec that includes a signature of the encoded bits.
   *
   * Same functionality as [[fixedSizeSignature]] with one difference -- the size of the signature bytes are
   * written between the encoded bits and the signature bits.
   *
   * @param size codec to use to encode/decode size of signature field
   * @param codec codec to use to encode/decode value field
   * @param signatureFactory factory to use for signing/verifying
   * @group crypto
   */
  def variableSizeSignature[A](size: Codec[Int])(codec: Codec[A])(implicit signatureFactory: SignatureFactory): Codec[A] =
    new SignatureCodec(codec, variableSizeBytes(size, BitVectorCodec))(signatureFactory)

  /**
   * Codec that encodes/decodes certificates using their default encoding.
   *
   * @param certType certificate type to pass to `java.security.cert.CertificateFactory.getInstance`
   * @group crypto
   */
  def certificate(certType: String): Codec[Certificate] = new CertificateCodec(certType)

  /**
   * Codec that encodes/decodes certificates using their default encoding.
   *
   * @group crypto
   */
  val x509Certificate: Codec[X509Certificate] =
    certificate("X.509").
      xmap[X509Certificate](_.asInstanceOf[X509Certificate], identity).
      withToString("x509certificate")

  /**
   * Provides the `|` method on `String` that allows creation of a named codec.
   *
   * Usage: {{{val codec = "id" | uint8}}}
   *
   * @group combinators
   */
  final implicit class StringEnrichedWithCodecNamingSupport(val name: String) extends AnyVal {
    /** Names the specified codec, resulting in the name being included in error messages. */
    def |[A](codec: Codec[A]): Codec[A] = new NamedCodec(name, codec)
  }

  // Tuple codec syntax

  /**
   * Type alias for Tuple2 in order to allow left nested tuples to be written as A ~ B ~ C ~ ....
   * @group tuples
   */
  final type ~[+A, +B] = (A, B)

  /**
   * Extractor that allows pattern matching on the tuples created by tupling codecs.
   * @group tuples
   */
  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = Some(t)
  }

  /**
   * Allows creation of left nested pairs by successive usage of `~` operator.
   * @group tuples
   */
  final implicit class ValueEnrichedWithTuplingSupport[A](val a: A) {
    def ~[B](b: B): (A, B) = (a, b)
  }

  /**
   * Allows use of a 2-arg function as a single arg function that takes a left-associated stack of pairs with 2 total elements.
   * @group tuples
   */
  final implicit def liftF2ToNestedTupleF[A, B, X](fn: (A, B) => X): ((A, B)) => X =
    fn.tupled

  /**
   * Allows use of a 3-arg function as a single arg function that takes a left-associated stack of pairs with 3 total elements.
   * @group tuples
   */
  final implicit def liftF3ToNestedTupleF[A, B, C, X](fn: (A, B, C) => X): (((A, B), C)) => X = {
    case a ~ b ~ c => fn(a, b, c)
  }

  /**
   * Allows use of a 4-arg function as a single arg function that takes a left-associated stack of pairs with 4 total elements.
   * @group tuples
   */
  final implicit def liftF4ToNestedTupleF[A, B, C, D, X](fn: (A, B, C, D) => X): ((((A, B), C), D)) => X = {
    case a ~ b ~ c ~ d => fn(a, b, c, d)
  }

  /**
   * Allows use of a 5-arg function as a single arg function that takes a left-associated stack of pairs with 5 total elements.
   * @group tuples
   */
  final implicit def liftF5ToNestedTupleF[A, B, C, D, E, X](fn: (A, B, C, D, E) => X): (((((A, B), C), D), E)) => X = {
    case a ~ b ~ c ~ d ~ e => fn(a, b, c, d, e)
  }

  /**
   * Allows use of a 6-arg function as a single arg function that takes a left-associated stack of pairs with 6 total elements.
   * @group tuples
   */
  final implicit def liftF6ToNestedTupleF[A, B, C, D, E, F, X](fn: (A, B, C, D, E, F) => X): ((((((A, B), C), D), E), F)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f => fn(a, b, c, d, e, f)
  }

  /**
   * Allows use of a 7-arg function as a single arg function that takes a left-associated stack of pairs with 7 total elements.
   * @group tuples
   */
  final implicit def liftF7ToNestedTupleF[A, B, C, D, E, F, G, X](fn: (A, B, C, D, E, F, G) => X): (((((((A, B), C), D), E), F), G)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f ~ g => fn(a, b, c, d, e, f, g)
  }

  /**
   * Allows use of an 8-arg function as a single arg function that takes a left-associated stack of pairs with 8 total elements.
   * @group tuples
   */
  final implicit def liftF8ToNestedTupleF[A, B, C, D, E, F, G, H, X](fn: (A, B, C, D, E, F, G, H) => X): ((((((((A, B), C), D), E), F), G), H)) => X = {
    case a ~ b ~ c ~ d ~ e ~ f ~ g ~ h => fn(a, b, c, d, e, f, g, h)
  }

  // DiscriminatorCodec syntax

  /**
   * Provides syntax for building a [[DiscriminatorCodec]].
   *
   * Usage: {{{
   val codecA: Codec[A] = ...
   val codecB: Codec[B] = ...

   val codecE: Codec[Either[A,B]] =
     discriminated[Either[A,B]].by(uint8)
     .| (0) { case Left(l) => l } (Left.apply) (codecA)
     .| (1) { case Right(r) => r } (Right.apply) (codecB)
     .build
   }}}

   This encodes an `Either[A,B]` by checking the given patterns
   in sequence from top to bottom. For the first pattern that matches,
   it emits the corresponding discriminator value: `0` for `Left`
   and `1` for `Right`, encoded via the `uint8` codec. It then emits
   either an encoded `A`, encoded using `codecA`, or an encoded `B`,
   using `codecB`.

   Decoding is the mirror of this; the returned `codecE` will first
   read an `Int`, using the `uint8` codec. If it is a `0`, it then
   runs `codecA`, and injects the result into `Either` via `Left.apply`.
   If it is a `1`, it runs `codecB` and injects the result into `Either`
   via `Right.apply`.

   There are a few variations on this syntax, depending on whether you
   have a `PartialFunction` from the base type or an `B => Option[S]`
   function from the base type to the subcase.

   If you you already have a codec specific to the case, you can omit
   the 'injection' function. For instance: {{{
     val leftCodec: Codec[Left[A,B]] = codecA.pxmap(Left.apply, Left.unapply)
     val rightCodec: Codec[Right[A,B]] = codecB.pxmap(Left.apply, Left.unapply)
     val codecE: Codec[Either[A,B]] =
       discriminated[Either[A,B]].by(uint8)
       .\ (0) { case l@Left(_) => l } (leftCodec) // backslash instead of '|'
       .\ (1) { case r@Right(_) => r } (rightCodec)
   }}}

   The actual formatted bits are identical with either formulation.
   * @group combinators
   */
  final def discriminated[A]: NeedDiscriminatorCodec[A] = new NeedDiscriminatorCodec[A] {
    final def by[B](discriminatorCodec: Codec[B]): DiscriminatorCodec[A, B] =
      new DiscriminatorCodec[A, B](discriminatorCodec, Vector())
  }

  /**
   * Provides a codec for an enumerated set of values, where each enumerated value is
   * mapped to a tag.
   *
   * @param discriminatorCodec codec used to encode/decode tag value
   * @param mappings mapping from tag values to/from enum values
   * @group combinators
   */
  final def mappedEnum[A, B](discriminatorCodec: Codec[B], mappings: (A, B)*): Codec[A] =
    mappedEnum(discriminatorCodec, mappings.toMap)

  /**
   * Provides a codec for an enumerated set of values, where each enumerated value is
   * mapped to a tag.
   *
   * @param discriminatorCodec codec used to encode/decode tag value
   * @param map mapping from tag values to/from enum values
   * @group combinators
   */
  final def mappedEnum[A, B](discriminatorCodec: Codec[B], map: Map[A, B]): Codec[A] = {
    map.foldLeft(discriminated[A].by(discriminatorCodec)) { case (acc, (value, tag)) =>
      acc.subcaseO(tag)(a => if (a == value) Some(a) else None)(provide(value))
    }
  }
}

