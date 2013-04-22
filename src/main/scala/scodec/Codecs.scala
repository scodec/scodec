package scodec

import java.nio.charset.Charset


object Codecs {

  val int8 = new IntCodec(8)
  val int16 = new IntCodec(16)
  val int24 = new IntCodec(24)
  val int32 = new IntCodec(32)
  val int64 = new LongCodec(64)

  val uint8 = new IntCodec(8, signed = false)
  val uint16 = new IntCodec(16, signed = false)
  val uint24 = new IntCodec(24, signed = false)
  val uint32 = new LongCodec(32, signed = false)


  def string(implicit charset: Charset) = new StringCodec(charset)
  val ascii = string(Charset.forName("US-ASCII"))
  val utf8 = string(Charset.forName("UTF-8"))
}
