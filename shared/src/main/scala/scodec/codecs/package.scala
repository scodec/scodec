package scodec

import scala.language.implicitConversions
import scala.math.{log10, floor}

import java.nio.charset.Charset
import java.security.cert.{ Certificate, X509Certificate }
import java.util.UUID
import java.util.zip.Deflater

import scodec.bits.{ BitVector, ByteOrdering, ByteVector }

import shapeless.{ HList, Nat, Sized }
import shapeless.syntax.sized._

/**
 * Provides codecs for common types and combinators for building larger codecs.
 *
 * === Bits and Bytes Codecs ===
 *
 * The simplest of the provided codecs are those that encode/decode `BitVector`s and `ByteVectors` directly.
 * These are provided by `bits` and `bytes` methods. These codecs encode all of the bits/bytes directly
 * in to the result and decode *all* of the remaining bits/bytes in to the result value. That is, the result
 * of `decode` always returns a empty bit vector for the remaining bits.
 *
 * Similarly, fixed size alternatives are provided by the `bits(size)` and `bytes(size)` methods, which
 * encode a fixed number of bits/bytes (or error if not provided the correct size) and decoded a fixed number
 * of bits/bytes (or error if that many bits/bytes are not available).
 *
 * There are more specialized codecs for working with bits, including `ignore` and `constant`.
 *
 *
 * === Numeric Codecs ===
 *
 * There are built-in codecs for `Int`, `Long`, `Float`, and `Double`.
 *
 * There are a number of predefined integral codecs named using the form: {{{
 [u]int$${size}[L]
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
 * Boolean values are supported by the `bool` codecs.
 *
 *
 * === Combinators ===
 *
 * There are a number of methods provided that create codecs out of other codecs. These include simple combinators
 * such as [[fixedSizeBits]] and [[variableSizeBits]] and advanced combinators such as [[discriminated]], which
 * provides its own DSL for building a large codec out of many small codecs. For a list of all combinators,
 * see the Combinators section below.
 *
 * === Cryptography Codecs ===
 *
 * There are codecs that support working with encrypted data ([[encrypted]]), digital signatures and checksums
 * ([[fixedSizeSignature]] and [[variableSizeSignature]]). Additionally, support for `java.security.cert.Certificate`s
 * is provided by [[certificate]] and [[x509Certificate]].
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
 * @groupname guards Guards
 * @groupprio guards 4
 *
 * @groupname tuples Tuple Support
 * @groupprio tuples 5
 *
 * @groupname logging Logging
 * @groupprio logging 6
 *
 * @groupname crypto Cryptography
 * @groupprio crypto 7
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
    def sizeBound = SizeBound.exact(size)
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
    private val codec = fixedSizeBytes(size.toLong, BitVectorCodec).xmap[ByteVector](_.toByteVector, _.toBitVector)
    def sizeBound = SizeBound.exact(size * 8L)
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
  val ushort8: Codec[Short] = new ShortCodec(8, false, ByteOrdering.BigEndian)

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
   * Codec for variable-length big-endian integers.
   * Encoding requires between 1 and 5 bytes, depending on the value.
   * Smaller ints require less bytes. Negative values are always encoded with 5 bytes.
   * @group numbers
   */
  val vint: Codec[Int] = new VarIntCodec(ByteOrdering.BigEndian)

  /**
   * Codec for variable-length little-endian integers.
   * Encoding requires between 1 and 5 bytes, depending on the value.
   * Smaller ints require less bytes. Negative values are always encoded with 5 bytes.
   * @group numbers
   */
  val vintL: Codec[Int] = new VarIntCodec(ByteOrdering.LittleEndian)

  /**
   * Codec for variable-length big-endian longs.
   * Encoding requires between 1 and 9 bytes, depending on the value.
   * Smaller longs require less bytes.
   * Negative values are not supported.
   * @group numbers
   */
  val vlong: Codec[Long] = new VarLongCodec(ByteOrdering.BigEndian)

  /**
   * Codec for variable-length packed decimal longs.
   * Negative values are not supported.
   * @group numbers
   */
  val vpbcd: Codec[Long] = VarPackedDecimalCodec

  /**
   * Codec for variable-length little-endian longs.
   * Encoding requires between 1 and 9 bytes, depending on the value.
   * Smaller longs require less bytes.
   * Negative values are not supported.
   * @group numbers
   */
  val vlongL: Codec[Long] = new VarLongCodec(ByteOrdering.LittleEndian)

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
   * Codec for n-nibble packed decimal (BCD) integers that are represented with `Long`.
   * @param nibbles number of nibbles (4-bit chunks)
   * @group numbers
   */
  def pbcd(nibbles: Int): Codec[Long] = fixedSizeBits(nibbles.toLong*4, vpbcd)

  /**
   * Codec for n-nibble packed decimal (BCD) integers that are represented with `Long`.
   * This codec, despite requiring the size in nibbles, is byte-size oriented.
   * This means it expects to parse complete bytes (even if nibble size is
   * odd). For encoding, this codec will pad 0s on the left while, for
   * decoding, it will fetch the size in bytes round up.
   * @param nibbles number of nibbles (4-bit chunks)
   * @group numbers
   */
  def lpbcd(nibbles: Int): Codec[Long] = new Codec[Long]{
    val nsize = nibbles.toLong * 4
    val bsize = nsize + nsize % 8
    def sizeBound = SizeBound.exact(bsize)
    def decode(b: BitVector) = fixedSizeBits(bsize, vpbcd).decode(b)
    def encode(l: Long) = fixedSizeBits(nsize, vpbcd).encode(l).map{x =>
      val size: Long = floor(log10(l.toDouble) + 1).toLong * 4

      (BitVector.low(bsize) ++ x.take(size)).drop(size)
    }
  }

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
    def sizeBound = SizeBound.exact(n)
    def encode(b: Boolean) = codec.encode(b)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"bool($n)"
  }

  /**
   * String codec that uses the implicit `Charset` to perform encoding/decoding.
   *
   * This codec does not encode the size of the string in to the output. Hence, decoding
   * a vector that has additional data after the encoded string will result in
   * unexpected output. Instead, it is common to use this codec along with either
   * [[fixedSizeBits]] or [[variableSizeBits]]. For example, a common encoding
   * is a size field, say 2 bytes, followed by the encoded string. This can be
   * accomplished with: {{{variableSizeBits(uint16, string)}}}
   *
   * @param charset charset to use to convert strings to/from binary
   * @group values
   */
  def string(implicit charset: Charset): Codec[String] = new StringCodec(charset)

  /**
   * String codec that uses the `US-ASCII` charset. See [[string]] for more information on `String` codecs.
   * @group values
   */
  val ascii = string(Platform.ascii)

  /**
   * String codec that uses the `UTF-8` charset. See [[string]] for more information on `String` codecs.
   * @group values
   */
  val utf8 = string(Platform.utf8)

  /**
   * String codec that uses the `US-ASCII` charset that encodes strings with a trailing `NUL` termination byte
   * and decodes a string up to the next `NUL` termination byte.
   * It fails to decode if the bit vector ends before a `NUL` termination byte can be found.
   * @group values
   */
  val cstring: Codec[String] = filtered(ascii, new Codec[BitVector] {
    val nul = BitVector.lowByte
    override def sizeBound: SizeBound = SizeBound.unknown
    override def encode(bits: BitVector): Attempt[BitVector] = Attempt.successful(bits ++ nul)
    override def decode(bits: BitVector): Attempt[DecodeResult[BitVector]] = {
      bits.bytes.indexOfSlice(nul.bytes) match {
        case -1 => Attempt.failure(Err("Does not contain a 'NUL' termination byte."))
        case i => Attempt.successful(DecodeResult(bits.take(i * 8L), bits.drop(i * 8L + 8L)))
      }
    }
  }).withToString("cstring")

  /**
   * String codec that uses the implicit `Charset` and prefixes the encoded string by the byte size
   * in a 32-bit 2s complement big endian field.
   *
   * @param charset charset to use to convert strings to/from binary
   * @group values
   */
  def string32(implicit charset: Charset): Codec[String] =
    variableSizeBytes(int32, string(charset)).withToString(s"string32(${charset.displayName})")

  /**
   * String codec that uses the implicit `Charset` and prefixes the encoded string by the byte size
   * in a 32-bit 2s complement little endian field.
   *
   * @param charset charset to use to convert strings to/from binary
   * @group values
   */
  def string32L(implicit charset: Charset): Codec[String] =
    variableSizeBytes(int32L, string(charset)).withToString(s"string32(${charset.displayName})")

  /**
   * String codec that uses the `US-ASCII` charset and prefixes the encoded string by the byte size
   * in a 32-bit 2s complement big endian field.
   * @group values
   */
  val ascii32 = string32(Platform.ascii)

  /**
   * String codec that uses the `US-ASCII` charset and prefixes the encoded string by the byte size
   * in a 32-bit 2s complement little endian field.
   * @group values
   */
  val ascii32L = string32L(Platform.ascii)

  /**
   * String codec that uses the `UTF-8` charset and prefixes the encoded string by the byte size
   * in a 32-bit 2s complement big endian field.
   * @group values
   */
  val utf8_32 = string32(Platform.utf8)

  /**
   * String codec that uses the `UTF-8` charset and prefixes the encoded string by the byte size
   * in a 32-bit 2s complement little endian field.
   * @group values
   */
  val utf8_32L = string32L(Platform.utf8)

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
    def sizeBound = fcodec.sizeBound
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
   * @param padCodec codec to use for padding
   * @group combinators
   */
  def paddedFixedSizeBits[A](size: Long, codec: Codec[A], padCodec: Codec[Unit]): Codec[A] = new PaddedFixedSizeCodec(size, codec, _ => padCodec)

  /**
   * Codec that limits the number of bits the specified codec works with.
   *
   * If the encoded result is larger than the specified
   * size, an encoding error is returned.
   *
   * If encoding with the specified codec
   * results in less than the specified size, the vector is right padded by repeatedly encoding with the
   * codec returned from `padCodec(numberOfPaddingBits)`.
   * An encoding error is returned if the padCodec result does not precisely fill the remaining space.
   *
   * When decoding, the specified codec is only given `size` bits. If the specified codec does not consume all the bits it was
   * given, all remaining bits are repeatedly decoded by the codec returned from `padCodec(remainingBitCount)`.
   * A decoding error is returned if any padding decode iteration returns an error.
   *
   * @param size number of bits
   * @param codec codec to limit
   * @param padCodec function that provides the codec to use for padding
   * @group combinators
   */
  def paddedFixedSizeBitsDependent[A](size: Long, codec: Codec[A], padCodec: Long => Codec[Unit]): Codec[A] = new PaddedFixedSizeCodec(size, codec, padCodec)

   /**
   * Byte equivalent of [[paddedFixedSizeBits]].
   * @param size number of bytes
   * @param codec codec to limit
   * @param padCodec codec to use for padding
   * @group combinators
   */
  def paddedFixedSizeBytes[A](size: Long, codec: Codec[A], padCodec: Codec[Unit]): Codec[A] = paddedFixedSizeBytesDependent(size, codec, _ => padCodec)

  /**
   * Byte equivalent of [[paddedFixedSizeBitsDependent]].
   *
   * The `padCodec` function is passed the number of *bits* of padding required -- not bytes.
   *
   * @param size number of bytes
   * @param codec codec to limit
   * @param padCodec function that provides the codec to use for padding
   * @group combinators
   */
   def paddedFixedSizeBytesDependent[A](size: Long, codec: Codec[A], padCodec: Long => Codec[Unit]): Codec[A] = new Codec[A] {
     private val fcodec = paddedFixedSizeBitsDependent(size * 8, codec, padCodec)
     def sizeBound = SizeBound.exact(size * 8)
     def encode(a: A) = fcodec.encode(a)
     def decode(b: BitVector) = fcodec.decode(b)
     override def toString = s"paddedFixedSizeBytes($size, $codec)"
   }

  /**
   * Codec that limits the number of bits the specified codec works with.
   *
   * When encoding, if encoding with the specified codec
   * results in less than the specified size, the vector is returned with no padding. If the result is larger than the specified
   * size, an encoding error is returned. This differs from `fixedSizeBits` by not padding encoded vectors less than the specified
   * size.
   *
   * When decoding, the specified codec is only given `size` bits. If the specified codec does not consume all the bits it was
   * given, any remaining bits are returned with the overall remainder.
   *
   * @param size number of bits
   * @param codec codec to limit
   * @group combinators
   */
  def limitedSizeBits[A](limit: Long, codec: Codec[A]): Codec[A] = new LimitedSizeCodec(limit, codec)

  /**
   * Byte equivalent of [[limitedSizeBits]].
   * @param size number of bytes
   * @param codec codec to limit
   * @group combinators
   */
  def limitedSizeBytes[A](limit: Long, codec: Codec[A]): Codec[A] = new Codec[A] {
    private val fcodec = limitedSizeBits(limit * 8, codec)
    def sizeBound = fcodec.sizeBound
    def encode(a: A) = fcodec.encode(a)
    def decode(b: BitVector) = fcodec.decode(b)
    override def toString = s"limitedSizeBytes($limit, $codec)"
  }

  /**
   * Codec that supports vectors of the form `size ++ value` where the `size` field decodes to the bit length of the `value` field.
   *
   * For example, encoding the string `"hello"` with `variableSizeBits(uint8, ascii)` yields a vector of 6 bytes -- the first byte being
   * 0x28 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * The `size` field can be any `Int` codec. An optional padding can be applied to the size field. The `sizePadding` is added to
   * the calculated size before encoding, and subtracted from the decoded size before decoding the value.
   *
   * For example, encoding `"hello"` with `variableSizeBits(uint8, ascii, 1)` yields a vector of 6 bytes -- the first byte being
   * 0x29 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bits to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizeBits[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    variableSizeBitsLong(widenIntToLong(size), value, sizePadding.toLong)

  /**
   * Byte equivalent of [[variableSizeBits]].
   * @param size codec that encodes/decodes the size in bytes
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bytes to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizeBytes[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    variableSizeBytesLong(widenIntToLong(size), value, sizePadding.toLong)

  private def widenIntToLong(c: Codec[Int]): Codec[Long] =
    c.widen[Long](i => i.toLong, l => if (l > Int.MaxValue || l < Int.MinValue) Attempt.failure(Err(s"$l cannot be converted to an integer")) else Attempt.successful(l.toInt)).withToString(c.toString)


  /**
   * Codec that supports vectors of the form `size ++ value` where the `size` field decodes to the bit length of the `value` field.
   *
   * For example, encoding the string `"hello"` with `variableSizeBitsLong(uint32, ascii)` yields a vector of 9 bytes -- the first four bytes being
   * 0x00000028 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * The `size` field can be any `Long` codec. An optional padding can be applied to the size field. The `sizePadding` is added to
   * the calculated size before encoding, and subtracted from the decoded size before decoding the value.
   *
   * For example, encoding `"hello"` with `variableSizeBitsLong(uint32, ascii, 1)` yields a vector of 9 bytes -- the first 4 bytes being
   * 0x00000029 and the next 5 bytes being the US-ASCII encoding of `"hello"`.
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
    private val codec = variableSizeBitsLong(size.widen[Long](_ * 8, bitsToBytesDivisible), value, sizePadding * 8)
    def sizeBound = size.sizeBound + value.sizeBound
    def encode(a: A) = codec.encode(a)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"variableSizeBytes($size, $value)"
  }

  private def bitsToBytesDivisible(n: Long): Attempt[Long] =
    if (n % 8 == 0) Attempt.successful(n / 8)
    else Attempt.failure(Err(s"$n is not evenly divisible by 8"))

  /**
   * Codec that supports vectors of the form `size ++ prefix ++ value` where the `size` field decodes to the bit length of the `value` field.
   *
   * For example, encoding `(3, "hello")` with `variableSizePrefixedBits(uint8, int32, ascii)` yields a vector of 10 bytes -- the first byte being
   * 0x28, the next 4 bytes being 0x00000003, and the last 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * The `size` field can be any `Int` codec. An optional padding can be applied to the size field. The `sizePadding` is added to
   * the calculated size before encoding, and subtracted from the decoded size before decoding the value.
   *
   * For example, encoding `(3, "hello")` with `variableSizePrefixedBits(uint8, int32, ascii, 1)` yields a vector of 10 bytes -- the first byte being
   * 0x29, the next 4 bytes being 0x00000003, and the last 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param prefix codec that encodes/decodes the prefix
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bits to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizePrefixedBits[A, B](size: Codec[Int], prefix: Codec[A], value: Codec[B], sizePadding: Int = 0): Codec[(A, B)] =
    variableSizePrefixedBitsLong(widenIntToLong(size), prefix, value, sizePadding.toLong)

  /**
   * Byte equivalent of [[variableSizePrefixedBits]].
   * @param size codec that encodes/decodes the size in bytes
   * @param prefix codec that encodes/decodes the prefix
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bytes to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizePrefixedBytes[A, B](size: Codec[Int], prefix: Codec[A], value: Codec[B], sizePadding: Int = 0): Codec[(A, B)] =
    variableSizePrefixedBytesLong(widenIntToLong(size), prefix, value, sizePadding.toLong)

  /**
   * Codec that supports vectors of the form `size ++ prefix ++ value` where the `size` field decodes to the bit length of the `value` field.
   *
   * For example, encoding the string `(3, "hello")` with `variableSizePrefixedBitsLong(uint32, int32, ascii)` yields a vector of 13 bytes -- the
   * first four bytes being 0x00000028, the next 4 bytes being 0x00000003, and the last 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * The `size` field can be any `Long` codec. An optional padding can be applied to the size field. The `sizePadding` is added to
   * the calculated size before encoding, and subtracted from the decoded size before decoding the value.
   *
   * For example, encoding `(3, "hello")` with `variableSizePrefixedBitsLong(uint32, int32, ascii, 1)` yields a vector of 13 bytes -- the first
   * 4 bytes being 0x00000029, the next 4 bytes being 0x00000003, and the last 5 bytes being the US-ASCII encoding of `"hello"`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param prefix codec that encodes/decodes the prefix
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bits to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizePrefixedBitsLong[A, B](size: Codec[Long], prefix: Codec[A], value: Codec[B], sizePadding: Long = 0): Codec[(A, B)] =
    new VariableSizePrefixedCodec(size, prefix, value, sizePadding)

  /**
   * Byte equivalent of [[variableSizePrefixedBitsLong]].
   * @param size codec that encodes/decodes the size in bytes
   * @param prefix codec that encodes/decodes the prefix
   * @param value codec the encodes/decodes the value
   * @param sizePadding number of bytes to add to the size before encoding (and subtract from the size before decoding)
   * @group combinators
   */
  def variableSizePrefixedBytesLong[A, B](size: Codec[Long], prefix: Codec[A], value: Codec[B], sizePadding: Long = 0): Codec[(A, B)] = new Codec[(A, B)] {
    private val codec = variableSizePrefixedBitsLong(size.widen[Long](_ * 8, bitsToBytesDivisible), prefix, value, sizePadding * 8)
    def sizeBound = size.sizeBound + value.sizeBound
    def encode(ab: (A, B)) = codec.encode(ab)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"variableSizePrefixedBytes($size, $prefix, $value)"
  }

  /**
   * Decodes using the specified codec but resets the remainder to the original vector.
   * Encodes with the specified codec.
   * @param target codec that encodes/decodes the value
   * @return codec that behaves the same as `target` but resets remainder to the input vector after decoding
   * @group combinators
   */
  def peek[A](target: Codec[A]): Codec[A] = new Codec[A] {
    def sizeBound = target.sizeBound
    def encode(a: A) = target.encode(a)
    def decode(b: BitVector) = target.decode(b).map { _.mapRemainder(_ => b) }
  }

  /**
   * Codec that decodes vectors of the form `size ++ rest` as a `BitVector`, where the returned vector includes the size bits.
   *
   * This differs from `variableSizeBits(size, bits, sizePadding)` in that the encoded size is expected to be encoded before
   * calling encode and the encoded size is returned as part of the vector.
   *
   * @param size size codec -- must have an exact size
   * @param sizePadding number of bits to subtract from the size before decoding
   */
  def peekVariableSizeBits(size: Codec[Int], sizePadding: Int = 0): Codec[BitVector] = peekVariableSizeBitsLong(widenIntToLong(size), sizePadding.toLong)

  /**
   * `Long` equivalent of [[peekVariableSizeBits]].
   * @param size size codec -- must have an exact size
   * @param sizePadding number of bits to subtract from the size before decoding
   */
  def peekVariableSizeBitsLong(size: Codec[Long], sizePadding: Long = 0L): Codec[BitVector] = new Codec[BitVector] {
    private val sizeInBits = size.sizeBound.exact.getOrElse(throw new IllegalArgumentException(s"must be used with a size field of an exactly known size but $size has size bound ${size.sizeBound}"))
    private val decoder = (peek(bits(sizeInBits)) ~ variableSizeBitsLong(size, bits, sizePadding)).map { case (sz, b) => sz ++ b }
    def sizeBound = size.sizeBound.atLeast
    def encode(b: BitVector) = Attempt.successful(b)
    def decode(b: BitVector) = decoder.decode(b)
    override def toString = s"peekVariableSizeBits($size)"
  }

  /**
   * Equivalent to [[peekVariableSizeBits]] where the size units are in bytes instead of bits.
   *
   * @param size size codec -- must have an exact size
   * @param sizePadding number of bytes to subtract from the size before decoding
   */
  def peekVariableSizeBytes(size: Codec[Int], sizePadding: Int = 0): Codec[BitVector] =
    peekVariableSizeBytesLong(widenIntToLong(size), sizePadding.toLong)

  /**
   * `Long` equivalent of [[peekVariableSizeBytes]].
   * @param size size codec -- must have an exact size
   * @param sizePadding number of bits to subtract from the size before decoding
   */
  def peekVariableSizeBytesLong(size: Codec[Long], sizePadding: Long = 0L): Codec[BitVector] = new Codec[BitVector] {
    private val codec = peekVariableSizeBitsLong(size.widen[Long](_ * 8, bitsToBytesDivisible), sizePadding * 8)
    def sizeBound = codec.sizeBound
    def encode(a: BitVector) = codec.encode(a)
    def decode(b: BitVector) = codec.decode(b)
    override def toString = s"peekVariableSizeBytes($size)"
  }

  /**
   * Codec that:
   *  - encodes using the specified codec but right-pads with 0 bits to the next largest byte when the size of the
   *    encoded bit vector is not divisible by 8
   *  - decodes using the specified codec but drops any leading bits of the remainder when the number of bytes
   *    consumed by the specified codec is not divisible by 8
   *
   * This combinator allows byte alignment without manually specifying ignore bits. For example, instead of writing
   * `(bool(1) :: bool(1) :: ignore(6)).dropUnits`, this combinator allows `byteAligned(bool(1) :: bool(1))`.
   *
   * Note that aligning large structures on byte boundaries can provide significant performance improvements when
   * converting to/from data structures that are based on bytes -- e.g., `Array[Byte]` or `ByteBuffer`.
   *
   * @param codec codec to align to next larger byte boundary
   * @group combinators
   */
  def byteAligned[A](codec: Codec[A]): Codec[A] = new ByteAlignedCodec(codec)

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
  def conditional[A](included: Boolean, codec: => Codec[A]): Codec[Option[A]] = new ConditionalCodec(included, codec)

  /**
   * Codec of `Option[A]` that delegates to a `Codec[A]` when the `guard` codec decodes a true.
   *
   * When encoding, a `Some` results in `guard` encoding a `true` and `target` encoding the value.
   * A `None` results in `guard` encoding a false and the `target` not encoding anything.
   *
   * Various guard codecs and combinators are provided by this library -- e.g., `bitsRemaining` and `recover`.
   *
   * @param guard codec that determines whether the target codec is included
   * @param target codec to conditionally include
   * @group combinators
   */
  def optional[A](guard: Codec[Boolean], target: Codec[A]): Codec[Option[A]] =
    either(guard, provide(()), target).
      xmap[Option[A]](_.right.toOption, _.toRight(())).
      withToString(s"optional($guard, $target)")

  /**
   * Codec that decodes true when the input vector is non-empty and false when it is empty.
   * Encodes to an empty bit vector.
   *
   * @group guards
   */
  val bitsRemaining: Codec[Boolean] = new Codec[Boolean] {
    def sizeBound = SizeBound.exact(0)
    def encode(b: Boolean) = Attempt.successful(BitVector.empty)
    def decode(b: BitVector) = Attempt.successful(DecodeResult(b.nonEmpty, b))
    override def toString = "bitsRemaining"
  }

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
      narrow[Vector[A]]({ case (cnt, xs) =>
        if (xs.size == cnt) Attempt.successful(xs)
        else Attempt.failure(Err(s"Insufficient number of elements: decoded ${xs.size} but should have decoded $cnt"))
      }, xs => (xs.size, xs)).
      withToString(s"vectorOfN($countCodec, $valueCodec)")

  /**
   * Codec that encodes/decodes a vector of `n` elements, where `n` is known at compile time.
   *
   * @param size number of elements in the vector
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  def sizedVector[A](size: Nat, codec: Codec[A])(implicit toInt: shapeless.ops.nat.ToInt[size.N]): Codec[Sized[Vector[A], size.N]] =
    vectorOfN(provide(toInt()), codec).xmapc(_.sized(size).get)(_.unsized).withToString(s"sizedVector(${toInt()}, $codec)")

  /**
   * Codec that encodes/decodes a `Vector[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the vector is encoded and all of the resulting bits are combined using `mux`.
   *
   * When decoding, `deMux` is called repeatedly to obtain the next bits (to decode using `valueCodec`) and the
   * remaining bits (input to `deMux` on next iteration) until a decoding error is encountered or no more bits remain.
   * The final return value is a vector of all decoded element values.
   *
   * Note: For large vectors, it may be necessary to compact bits in `deMux`.
   *
   * @param mux element multiplexer
   * @param deMux element de-multiplexer (should return the next bits to decode and the remaining bits for next iteration)
   * @param valueCodec element codec (used to decode next bits)
   * @tparam A element type
   * @group combinators
   */
  def vectorMultiplexed[A](mux: (BitVector, BitVector) => BitVector, deMux: BitVector => (BitVector, BitVector), valueCodec: Codec[A]): Codec[Vector[A]] =
    new VectorMultiplexedCodec[A](mux, deMux, valueCodec)

  /**
   * Codec that encodes/decodes a `Vector[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the vector is encoded and all of the resulting bits are concatenated using `delimiter`.
   *
   * When decoding, the input bits are first (logically) grouped into `delimiter` sized chunks and partitioned around `delimiter` chunks.
   * Then, the individual partitions are (concatenated and) decoded using the `valueCodec` and the values collected are returned in a vector.
   *
   * Note: This method applies specific semantics to the notion of a `delimiter`. An alternate (and faster) implementation could be to search
   * for the `delimiter` using `BitVector.indexOfSlice` but this would work only if value bits do not contain the `delimiter` bits at
   * any bit position.
   *
   * Example:
   * {{{
   * val codec = vectorDelimited(BitVector(' '), ascii)
   * codec.decode(ascii.encode("i am delimited").require).require.value // Vector("i", "am", "delimited")
   * }}}
   *
   * @param delimiter the bits used to separate element bit values
   * @param valueCodec element codec (used to decode next bits)
   * @tparam A element type
   * @group combinators
   */
  def vectorDelimited[A](delimiter: BitVector, valueCodec: Codec[A]): Codec[Vector[A]] =
    if (delimiter.size == 0) vector(valueCodec)
    else vectorMultiplexed(
      _ ++ delimiter ++ _,
      bits => DeMultiplexer.delimited(bits, delimiter),
      valueCodec).withToString(s"vectorDelimited($delimiter, $valueCodec)")

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
      narrow[List[A]]({ case (cnt, xs) =>
        if (xs.size == cnt) Attempt.successful(xs)
        else Attempt.failure(Err(s"Insufficient number of elements: decoded ${xs.size} but should have decoded $cnt"))
      }, xs => (xs.size, xs)).
      withToString(s"listOfN($countCodec, $valueCodec)")

  /**
   * Codec that encodes/decodes a list of `n` elements, where `n` is known at compile time.
   *
   * @param size number of elements in the list
   * @param codec codec to encode/decode a single element of the sequence
   * @group combinators
   */
  def sizedList[A](size: Nat, codec: Codec[A])(implicit toInt: shapeless.ops.nat.ToInt[size.N]): Codec[Sized[List[A], size.N]] =
    listOfN(provide(toInt()), codec).xmapc(_.sized(size).get)(_.unsized).withToString(s"sizedList(${toInt()}, $codec)")

  /**
   * Codec that encodes/decodes a `List[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the list is encoded and all of the resulting bits are combined using `mux`.
   *
   * When decoding, `deMux` is called repeatedly to obtain the next bits (to decode using `valueCodec`) and the
   * remaining bits (input to `deMux` on next iteration) until a decoding error is encountered or no more bits remain.
   * The final return value is a list of all decoded element values.
   *
   * Note: For large lists, it may be necessary to compact bits in `deMux`.
   *
   * @param mux element multiplexer
   * @param deMux element de-multiplexer (should return the next bits to decode and the remaining bits for next iteration)
   * @param valueCodec element codec (used to decode next bits)
   * @tparam A element type
   * @group combinators
   */
  def listMultiplexed[A](mux: (BitVector, BitVector) => BitVector, deMux: BitVector => (BitVector, BitVector), valueCodec: Codec[A]): Codec[List[A]] =
    new ListMultiplexedCodec[A](mux, deMux, valueCodec)

  /**
   * Codec that encodes/decodes a `List[A]` from a `Codec[A]`.
   *
   * When encoding, each `A` in the list is encoded and all of the resulting bits are concatenated using `delimiter`.
   *
   * When decoding, the input bits are first (logically) grouped into `delimiter` sized chunks and partitioned around `delimiter` chunks.
   * Then, the individual partitions are (concatenated and) decoded using the `valueCodec` and the values collected are returned in a list.
   *
   * Note: This method applies specific semantics to the notion of a `delimiter`. An alternate (and faster) implementation could be to search
   * for the `delimiter` using `BitVector.indexOfSlice` but this would work only if value bits do not contain the `delimiter` bits at
   * any bit position.
   *
   * Example:
   * {{{
   * val codec = listDelimited(BitVector(' '), ascii)
   * codec.decode(ascii.encode("i am delimited").require).require.value // List("i", "am", "delimited")
   * }}}
   *
   * @param delimiter the bits used to separate element bit values
   * @param valueCodec element codec (used to decode next bits)
   * @tparam A element type
   * @group combinators
   */
  def listDelimited[A](delimiter: BitVector, valueCodec: Codec[A]): Codec[List[A]] =
    if (delimiter.size == 0) list(valueCodec)
    else listMultiplexed(
      _ ++ delimiter ++ _,
      bits => DeMultiplexer.delimited(bits, delimiter),
      valueCodec).withToString(s"listDelimited($delimiter, $valueCodec)")

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
   * Either codec that supports bit vectors of form `indicator ++ (left or right)` where a
   * value of `false` for the indicator indicates it is followed by a left value and a value
   * of `true` indicates it is followed by a right value.
   *
   * @param indicator codec that encodes/decodes false for left and true for right
   * @param left codec the encodes a left value
   * @param right codec the encodes a right value
   * @group combinators
   */
  def either[L, R](indicator: Codec[Boolean], left: Codec[L], right: Codec[R]): Codec[Either[L, R]] =
    discriminated[Either[L, R]].by(indicator)
    .| (false) { case Left(l)  => l } (Left.apply) (left)
    .| (true)  { case Right(r) => r } (Right.apply) (right)

  /**
   * Either codec that supports bit vectors of form `left or right` where the right codec
   * is consulted first when decoding. If the right codec fails to decode, the left codec
   * is used.
   *
   * @param left codec the encodes a left value
   * @param right codec the encodes a right value
   * @group combinators
   */
  def fallback[L, R](left: Codec[L], right: Codec[R]): Codec[Either[L, R]] = new Codec[Either[L, R]] {
    def sizeBound = left.sizeBound | right.sizeBound
    def encode(e: Either[L, R]) = e.fold(left.encode, right.encode)
    def decode(b: BitVector) = right.decode(b).map(_.map(Right(_))).recoverWith {
      case _ => left.decode(b).map(_.map(Left(_)))
    }
  }

  /**
   * Provides a `Codec[A]` that delegates to a lazily evaluated `Codec[A]`.
   * @group combinators
   */
  def lazily[A](codec: => Codec[A]): Codec[A] = Codec.lazily(codec)

  /**
   * Codec that always fails encoding and decoding with the specified message.
   *
   * @group combinators
   */
  def fail[A](err: Err): Codec[A] = fail(err, err)

  /**
   * Codec that always fails encoding and decoding with the specified messages.
   *
   * @group combinators
   */
  def fail[A](encErr: Err, decErr: Err): Codec[A] = new FailCodec[A](encErr, decErr)

  /**
   * Codec that compresses the results of encoding with the specified codec and decompresses prior to decoding with the specified codec.
   *
   * Compression is performed using ZLIB. There are a number of defaulted parameters that control compression specifics.
   *
   * @param level compression level, 0-9, with 0 disabling compression and 9 being highest level of compression -- see `java.util.zip.Deflater` for details
   * @param strategy compression strategy -- see `java.util.zip.Deflater` for details
   * @param nowrap if true, ZLIB header and checksum will not be used
   * @param chunkSize buffer size, in bytes, to use when compressing
   * @group combinators
   */
  def zlib[A](codec: Codec[A], level: Int = Deflater.DEFAULT_COMPRESSION, strategy: Int = Deflater.DEFAULT_STRATEGY, nowrap: Boolean = false, chunkSize: Int = 4096): Codec[A] =
    new ZlibCodec(codec, level, strategy, nowrap, chunkSize)

  /**
   * Codec that filters bits before/after decoding/encoding.
   *
   * Note: the remainder returned from `filter.decode` is appended to the remainder of `codec.decode`.
   *
   * @param codec the target codec
   * @param filter a codec that represents pre/post-processing stages for input/output bits
   * @group combinators
   */
  def filtered[A](codec: Codec[A], filter: Codec[BitVector]): Codec[A] = new Codec[A] {
    def sizeBound: SizeBound = filter.sizeBound
    def encode(value: A): Attempt[BitVector] = codec.encode(value) flatMap filter.encode
    def decode(bits: BitVector): Attempt[DecodeResult[A]] =
      filter.decode(bits).flatMap(r => codec.decode(r.value).map(_.mapRemainder(_ ++ r.remainder)))
    override def toString = s"filtered($codec, $filter)"
  }

  /**
   * Codec that supports a checksum.
   *
   * When encoding, first the value is encoded using `target`, then a checksum is computed over the result the encoded value using `checksum`,
   * and finally, the encoded value and the checksum are converted to a single vector using `framing.encode(value -> chk)`.
   *
   * When decoding, the input vector is split in to an encoded value, a checksum value, and a remainder using `framing.decode`.
   * If `validate` is true, a checksum is computed over the encoded value and compared with the decoded checksum value. If the checksums
   * match, the encoded value is decoded with `target` and the result is returned, with its remainder concatenated with the remainder of
   * deframing. If the checksums do not match, a `ChecksumMismatch` error is raised.
   *
   * For example: {{{
     val crc32 = scodec.bits.crc(hex"04c11db7".bits, hex"ffffffff".bits, true, true, hex"ffffffff".bits)

     // Size of the string is not included in the checksum -- the `framing` codec handles adding the size *after* checksum computation
     val c = checksummed(utf8, crc32, variableSizeBytes(int32, bits) ~ bits(32))

     // Size of the string is included in the checksum
     val d = checksummed(utf8_32, crc32, peekVariableSizeBytes(int32) ~ bits(32))
   }}}
   *
   * @param target codec used for encoding/decoding values of type `A`
   * @param checksum computes a checksum of the input
   * @param framing codec used to convert the encoded value and computed checksum in to a single vector
   * @group combinators
   */
  def checksummed[A](target: Codec[A], checksum: BitVector => BitVector, framing: Codec[(BitVector, BitVector)], validate: Boolean = true): Codec[A] = new Codec[A] {
    def sizeBound: SizeBound = target.sizeBound.atLeast
    def encode(a: A) = for {
      value <- target.encode(a)
      result <- framing.encode(value -> checksum(value))
    } yield result
    def decode(bits: BitVector) = for {
      r <- framing.decode(bits)
      (value, actual) = r.value
      result <- {
        if (validate) {
          val expected = checksum(value)
          if (expected == actual) target.decode(value)
          else Attempt.failure(ChecksumMismatch(value, expected, actual))
        } else {
          target.decode(value)
        }
      }
    } yield result.mapRemainder { _ ++ r.remainder }
    override def toString = s"checksummed($target, $framing)"
  }

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
   * Use [[SignatureFactory]] or [[ChecksumFactory]] to create a [[SignerFactory]].
   *
   * @param size size in bytes of signature
   * @param codec codec to use to encode/decode value field
   * @param signatureFactory factory to use for signing/verifying
   * @group crypto
   */
  def fixedSizeSignature[A](size: Int)(codec: Codec[A])(implicit signerFactory: SignerFactory): Codec[A] =
    new SignatureCodec(codec, fixedSizeBytes(size.toLong, BitVectorCodec))(signerFactory)

  /**
   * Codec that includes a signature of the encoded bits.
   *
   * Same functionality as [[fixedSizeSignature]] with one difference -- the size of the signature bytes are
   * written between the encoded bits and the signature bits.
   *
   * Use [[SignatureFactory]] or [[ChecksumFactory]] to create a [[SignerFactory]].
   *
   * @param size codec to use to encode/decode size of signature field
   * @param codec codec to use to encode/decode value field
   * @param signatureFactory factory to use for signing/verifying
   * @group crypto
   */
  def variableSizeSignature[A](size: Codec[Int])(codec: Codec[A])(implicit signerFactory: SignerFactory): Codec[A] =
    new SignatureCodec(codec, variableSizeBytes(size, BitVectorCodec))(signerFactory)

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
  def x509Certificate: Codec[X509Certificate] =
    certificate("X.509").
      xmap[X509Certificate](_.asInstanceOf[X509Certificate], identity).
      withToString("x509certificate")

  /**
   * Provides the `|` method on `String`, which is reverse syntax for `codec withContext ctx`.
   *
   * Usage: {{{val codec = "id" | uint8}}}
   *
   * @group combinators
   */
  final implicit class StringEnrichedWithCodecContextSupport(val context: String) extends AnyVal {
    /** Pushes context into the specified codec. */
    def |[A](codec: Codec[A]): Codec[A] = codec withContext context
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
   }}}

   * This encodes an `Either[A,B]` by checking the given patterns
   * in sequence from top to bottom. For the first pattern that matches,
   * it emits the corresponding discriminator value: `0` for `Left`
   * and `1` for `Right`, encoded via the `uint8` codec. It then emits
   * either an encoded `A`, encoded using `codecA`, or an encoded `B`,
   * using `codecB`.
   *
   * Decoding is the mirror of this; the returned `codecE` will first
   * read an `Int`, using the `uint8` codec. If it is a `0`, it then
   * runs `codecA`, and injects the result into `Either` via `Left.apply`.
   * If it is a `1`, it runs `codecB` and injects the result into `Either`
   * via `Right.apply`.
   *
   * There are a few variations on this syntax. See [[DiscriminatorCodec]] for details.
   *
   * @group combinators
   */
  def discriminated[A]: NeedDiscriminatorCodec[A] = new NeedDiscriminatorCodec[A] {
    final def by[B](discriminatorCodec: Codec[B]): DiscriminatorCodec[A, B] =
      new DiscriminatorCodec[A, B](discriminatorCodec, Vector(), CodecTransformation.Id)
  }

  /**
   * Provides a codec for an enumerated set of values, where each enumerated value is
   * mapped to a tag.
   *
   * @param discriminatorCodec codec used to encode/decode tag value
   * @param mappings mapping from tag values to/from enum values
   * @group combinators
   */
  def mappedEnum[A, B](discriminatorCodec: Codec[B], mappings: (A, B)*): DiscriminatorCodec[A, B] =
    mappedEnum(discriminatorCodec, mappings.toMap)

  /**
   * Provides a codec for an enumerated set of values, where each enumerated value is
   * mapped to a tag.
   *
   * @param discriminatorCodec codec used to encode/decode tag value
   * @param map mapping from tag values to/from enum values
   * @group combinators
   */
  def mappedEnum[A, B](discriminatorCodec: Codec[B], map: Map[A, B]): DiscriminatorCodec[A, B] = {
    map.foldLeft(discriminated[A].by(discriminatorCodec)) { case (acc, (value, tag)) =>
      acc.subcaseO(tag)(a => if (a == value) Some(a) else None)(provide(value))
    }
  }

  /**
   * Alternative to [[fallback]] that only falls back to left codec when the right codec fails to decode
   * due to an unknown discriminator (i.e., `KnownDiscriminatorType[_]#UnknownDiscriminator`).
   *
   * @param left codec to use when the right codec fails due to an unknown discriminator error
   * @param right codec to use by default when decoding
   * @group combinators
   */
  def discriminatorFallback[L, R](left: Codec[L], right: Codec[R]): Codec[Either[L, R]] = new Codec[Either[L, R]] {
    def sizeBound = left.sizeBound | right.sizeBound
    def encode(e: Either[L, R]) = e.fold(left.encode, right.encode)
    def decode(b: BitVector) = right.decode(b).map(_.map(Right(_))).recoverWith {
      case _: KnownDiscriminatorType[_]#UnknownDiscriminator => left.decode(b).map(_.map(Left(_)))
    }
  }

  /**
   * Codec for an `Enumeration` that encodes/decodes using `Enumeration.Value.id` values.
   *
   * @param discriminator the codec for `Enumeration.Value.id` values
   * @param enumeration the target `Enumeration`
   * @return
   */
  def enumerated(discriminator: Codec[Int], enumeration: Enumeration) =
    scodec.codecs.mappedEnum(discriminator, enumeration.values.map(e => e -> e.id).toMap)

  /**
   * Converts an `HList` of codecs in to a single codec.
   * That is, converts `Codec[X0] :: Codec[X1] :: ... :: Codec[Xn] :: HNil` in to a
   * `Codec[X0 :: X1 :: ... :: Xn :: HNil].
   * @group combinators
   */
  def hlist[L <: HList](l: L)(implicit toHListCodec: ToHListCodec[L]): toHListCodec.Out = toHListCodec(l)

  /**
   * Wraps a codec and adds logging of each encoding and decoding operation.
   *
   * The `logEncode` and `logDecode` functions are called with the result of each encoding and decoding
   * operation.
   *
   * This method is intended to be used to build a domain specific logging combinator. For example: {{{
   * def log[A] = logBuilder[A]((a, r) => myLogger.debug(s"..."), (b, r) => myLogger.debug(s"...")) _
   * ...
   * log(myCodec)
   * }}}
   *
   * For quick logging to standard out, consider using [[logFailuresToStdOut]].
   *
   * @group logging
   */
  def logBuilder[A](logEncode: (A, Attempt[BitVector]) => Unit, logDecode: (BitVector, Attempt[DecodeResult[A]]) => Unit)(codec: Codec[A]): Codec[A] = new Codec[A] {
    override def sizeBound = codec.sizeBound
    override def encode(a: A) = {
      val res = codec.encode(a)
      logEncode(a, res)
      res
    }
    override def decode(b: BitVector) = {
      val res = codec.decode(b)
      logDecode(b, res)
      res
    }
    override def toString = codec.toString
  }

  private val constUnit: Any => Unit = _ => ()

  /**
   * Variant of [[logBuilder]] that only logs successful results.
   * @group logging
   */
  def logSuccessesBuilder[A](logEncode: (A, BitVector) => Unit, logDecode: (BitVector, DecodeResult[A]) => Unit)(codec: Codec[A]): Codec[A] =
    logBuilder[A]((a, r) => r.fold(constUnit, logEncode(a, _)), (b, r) => r.fold(constUnit, logDecode(b, _)))(codec)

  /**
   * Variant of [[logBuilder]] that only logs failed results.
   * @group logging
   */
  def logFailuresBuilder[A](logEncode: (A, Err) => Unit, logDecode: (BitVector, Err) => Unit)(codec: Codec[A]): Codec[A] =
    logBuilder[A]((a, r) => r.fold(logEncode(a, _), constUnit), (b, r) => r.fold(logDecode(b, _), constUnit))(codec)

  /**
   * Combinator intended for use in debugging that logs all encoded values and decoded values to standard out.
   *
   * @param prefix prefix string to include in each log statement
   * @group logging
   */
  def logToStdOut[A](codec: Codec[A], prefix: String = ""): Codec[A] = {
    val pfx = if (prefix.isEmpty) "" else s"$prefix: "
    logBuilder[A]((a, r) => println(s"${pfx}encoded $a to $r"), (b, r) => println(s"${pfx}decoded $b to $r"))(codec)
  }

  /**
   * Combinator intended for use in debugging that logs all failures while encoding or decoding to standard out.
   *
   * @param prefix prefix string to include in each log statement
   * @group logging
   */
  def logFailuresToStdOut[A](codec: Codec[A], prefix: String = ""): Codec[A] = {
    val pfx = if (prefix.isEmpty) "" else s"$prefix: "
    logFailuresBuilder[A]((a, e) => println(s"${pfx}failed to encode $a: $e"), (b, e) => println(s"${pfx}failed to decode $b: $e"))(codec)
  }

  /**
   * Codec that ensures variable size data is constrained within a minSize and maxSize bounds.
   *
   * This means that the size is variable only within a limited range. It will work just as variableSizeBytes codec,
   * but ensuring that the binary data is at least `minSize` bytes long and at most `maxSize` bytes long.
   *
   * The `minSize` has the default value of `0`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param minSize minimum size in bytes that the message can have
   * @param maxSize maximum size in bytes that the message can have
   * @group combinators
   */
  def constrainedVariableSizeBytes[A](size: Codec[Int], value: Codec[A], minSize: Int, maxSize: Int): Codec[A] =
    new ConstrainedVariableSizeCodec(widenIntToLong(size), value, minSize.toLong, maxSize.toLong)

  /**
   * Codec that ensures variable size data is constrained within a minSize and maxSize bounds.
   *
   * This means that the size is variable only within a limited range. It will work just as variableSizeBytes codec,
   * but ensuring that the binary data is at least `minSize` bytes long and at most `maxSize` bytes long.
   *
   * The `minSize` has the default value of `0`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param maxSize maximum size in bytes that the message can have
   * @param minSize minimum size in bytes that the message can have
   * @group combinators
   */
  def constrainedVariableSizeBytesLong[A](size: Codec[Long], value: Codec[A], minSize: Long, maxSize: Long): Codec[A] =
    new ConstrainedVariableSizeCodec(size, value, minSize, maxSize)

  /**
   * Codec that ensures variable size data is constrained within a minSize and maxSize bounds.
   *
   * This means that the size is variable only within a limited range. It will work just as variableSizeBytes codec,
   * but ensuring that the binary data is at least `minSize` bytes long and at most `maxSize` bytes long.
   *
   * The `minSize` has the default value of `0`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param maxSize maximum size in bytes that the message can have
   * @group combinators
   */
  def constrainedVariableSizeBytes[A](size: Codec[Int], value: Codec[A], maxSize: Int): Codec[A] =
    new ConstrainedVariableSizeCodec(widenIntToLong(size), value, 0L, maxSize.toLong)

  /**
   * Codec that ensures variable size data is constrained within a minSize and maxSize bounds.
   *
   * This means that the size is variable only within a limited range. It will work just as variableSizeBytes codec,
   * but ensuring that the binary data is at least `minSize` bytes long and at most `maxSize` bytes long.
   *
   * The `minSize` has the default value of `0`.
   *
   * @param size codec that encodes/decodes the size in bits
   * @param value codec the encodes/decodes the value
   * @param maxSize maximum size in bytes that the message can have
   * @group combinators
   */
  def constrainedVariableSizeBytesLong[A](size: Codec[Long], value: Codec[A], maxSize: Long): Codec[A] =
    new ConstrainedVariableSizeCodec(size, value, 0, maxSize)

  /** Provides common implicit codecs. */
  object implicits extends ImplicitCodecs
}
