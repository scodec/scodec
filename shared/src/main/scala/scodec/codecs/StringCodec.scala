package scodec
package codecs

import java.nio.{ ByteBuffer, CharBuffer }
import java.nio.charset.Charset
import java.nio.charset.{ MalformedInputException, UnmappableCharacterException }

import scodec.bits.BitVector

private[codecs] final class StringCodec(charset: Charset) extends Codec[String] {

  override def sizeBound = SizeBound.unknown

  override def encode(str: String) = {
    val encoder = charset.newEncoder
    val buffer = CharBuffer.wrap(str)
    try Attempt.successful(BitVector(encoder.encode(buffer)))
    catch {
      case (_: MalformedInputException | _: UnmappableCharacterException) =>
        Attempt.failure(Err(s"${charset.displayName} cannot encode character '${buffer.charAt(0)}'"))
    }
  }

  override def decode(buffer: BitVector) = {
    val decoder = charset.newDecoder
    try {
      val asBuffer = ByteBuffer.wrap(buffer.toByteArray)
      Attempt.successful(DecodeResult(decoder.decode(asBuffer).toString, BitVector.empty))
    } catch {
      case (_: MalformedInputException | _: UnmappableCharacterException) =>
        Attempt.failure(Err(s"${charset.displayName} cannot decode string from '0x${buffer.toByteVector.toHex}'"))
    }
  }

  override def toString = charset.displayName
}
