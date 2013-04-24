package scodec

import java.nio.charset.Charset


object Codecs extends NamedCodecSyntax with TupleCodecSyntax with HListCodecSyntax {

  val int8: Codec[Int] = new IntCodec(8)
  val int16: Codec[Int] = new IntCodec(16)
  val int24: Codec[Int] = new IntCodec(24)
  val int32: Codec[Int] = new IntCodec(32)
  val int64: Codec[Long] = new LongCodec(64)

  val uint2: Codec[Int] = new IntCodec(4, signed = false)
  val uint4: Codec[Int] = new IntCodec(4, signed = false)
  val uint8: Codec[Int] = new IntCodec(8, signed = false)
  val uint16: Codec[Int] = new IntCodec(16, signed = false)
  val uint24: Codec[Int] = new IntCodec(24, signed = false)
  val uint32: Codec[Long] = new LongCodec(32, signed = false)

  def bool: Codec[Boolean] = BooleanCodec

  def string(implicit charset: Charset): Codec[String] = new StringCodec(charset)
  val ascii = string(Charset.forName("US-ASCII"))
  val utf8 = string(Charset.forName("UTF-8"))

  def ignore(bits: Int): Codec[Unit] = new IgnoreCodec(bits)

  def constant(bits: BitVector): Codec[Unit] = new ConstantCodec(bits)
  def constant[A: Integral](bits: A*): Codec[Unit] = new ConstantCodec(BitVector(bits: _*))

  implicit val unitInstance = scalaz.std.anyVal.unitInstance
}
