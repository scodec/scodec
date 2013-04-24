package scodec

import java.nio.charset.Charset

import shapeless.Iso


object Codecs extends NamedCodecSyntax with TupleCodecSyntax with HListCodecSyntax {

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

  def int(bits: Int): Codec[Int] = new IntCodec(bits)
  def uint(bits: Int): Codec[Int] = new IntCodec(bits, signed = false)
  def long(bits: Int): Codec[Long] = new LongCodec(bits)
  def ulong(bits: Int): Codec[Long] = new LongCodec(bits, signed = false)

  val bool: Codec[Boolean] = BooleanCodec

  def string(implicit charset: Charset): Codec[String] = new StringCodec(charset)
  val ascii = string(Charset.forName("US-ASCII"))
  val utf8 = string(Charset.forName("UTF-8"))

  def ignore(bits: Int): Codec[Unit] = new IgnoreCodec(bits)

  def constant(bits: BitVector): Codec[Unit] = new ConstantCodec(bits)
  def constant[A: Integral](bits: A*): Codec[Unit] = new ConstantCodec(BitVector(bits: _*))

  def fixedSizeBits[A](size: Int, codec: Codec[A]): Codec[A] = new FixedSizeCodec(size, codec)
  def fixedSizeBytes[A](size: Int, codec: Codec[A]): Codec[A] = fixedSizeBits(size * 8, codec)
  def variableSizeBits[A](size: Codec[Int], value: Codec[A]): Codec[A] = new VariableSizeCodec(size, value)
  def variableSizeBytes[A](size: Codec[Int], value: Codec[A]): Codec[A] = variableSizeBits(size.xmap(_ * 8, _ / 8), value)

  def bits(size: Int): Codec[BitVector] = fixedSizeBits(size, BitVectorCodec)
  def bytes(size: Int): Codec[BitVector] = fixedSizeBytes(size, BitVectorCodec)

  def conditional[A](included: Boolean, codec: Codec[A]): Codec[Option[A]] = new ConditionalCodec(included, codec)

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
