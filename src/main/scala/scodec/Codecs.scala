package scodec

import java.nio.charset.Charset
import java.security.cert.Certificate
import java.util.UUID

import shapeless.Iso


object Codecs extends NamedCodecSyntax with TupleCodecSyntax with HListCodecSyntax with DiscriminatorCodecSyntax {

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

  def ignore(bits: Int): Codec[Unit] = new IgnoreCodec(bits)

  def constant(bits: BitVector): Codec[Unit] = new ConstantCodec(bits)
  def constant[A: Integral](bits: A*): Codec[Unit] = new ConstantCodec(BitVector(bits: _*))

  def fixedSizeBits[A](size: Int, codec: Codec[A]): Codec[A] = new FixedSizeCodec(size, codec)
  def fixedSizeBytes[A](size: Int, codec: Codec[A]): Codec[A] = fixedSizeBits(size * 8, codec).withToString(s"fixedSizeBytes($size, $codec)")

  def variableSizeBits[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    new VariableSizeCodec(size, value, sizePadding)
  def variableSizeBytes[A](size: Codec[Int], value: Codec[A], sizePadding: Int = 0): Codec[A] =
    variableSizeBits(size.xmap[Int](_ * 8, _ / 8).withToString(size.toString), value, sizePadding * 8).withToString(s"variableSizeBytes($size, $value)")

  def bits(size: Int): Codec[BitVector] = fixedSizeBits(size, BitVectorCodec).withToString(s"bits($size)")
  def bytes(size: Int): Codec[ByteVector] = fixedSizeBytes(size, BitVectorCodec).xmap[ByteVector](_.toByteVector, _.toBitVector).withToString(s"bytes($size)")

  def conditional[A](included: Boolean, codec: Codec[A]): Codec[Option[A]] = new ConditionalCodec(included, codec)

  def repeated[A](codec: Codec[A]): Codec[collection.immutable.IndexedSeq[A]] = new IndexedSeqCodec(codec)

  def provide[A](value: A): Codec[A] = new ProvideCodec(value)

  def encrypted[A](codec: Codec[A])(implicit cipherFactory: CipherFactory): Codec[A] = new CipherCodec(codec)(cipherFactory)

  def fixedSizeSignature[A](byteSize: Int)(codec: Codec[A])(implicit signatureFactory: SignatureFactory): Codec[A] =
    new SignatureCodec(codec, fixedSizeBytes(byteSize, BitVectorCodec))(signatureFactory)

  def variableSizeSignature[A](byteSizeCodec: Codec[Int])(codec: Codec[A])(implicit signatureFactory: SignatureFactory): Codec[A] =
    new SignatureCodec(codec, variableSizeBytes(byteSizeCodec, BitVectorCodec))(signatureFactory)

  val x509Certificate: Codec[Certificate] = new CertificateCodec("X.509")

  // Needed for the ignore combinator when used with <~, ~>, and :~>:
  implicit val unitInstance = scalaz.std.anyVal.unitInstance

  def isoFromFunctions[A, B](to: A => B, from: B => A): Iso[A, B] = {
    val toFn = to
    val fromFn = from
    new Iso[A, B] {
      def to(a: A) = toFn(a)
      def from(b: B) = fromFn(b)
    }
  }
}
