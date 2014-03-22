package scodec
package codecs

import scalaz.\/

import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.{MalformedInputException, UnmappableCharacterException}

import scodec.bits.BitVector

private[codecs] final class StringCodec(charset: Charset) extends Codec[String] {

  override def encode(str: String) = {
    val encoder = charset.newEncoder
    val buffer = CharBuffer.wrap(str)
    try \/.right(BitVector(encoder.encode(buffer)))
    catch {
      case (_: MalformedInputException | _: UnmappableCharacterException) =>
        \/.left(s"${charset.displayName} cannot encode character '${buffer.charAt(0)}'")
    }
  }

  override def decode(buffer: BitVector) = {
    val decoder = charset.newDecoder
    try {
      \/.right((BitVector.empty, decoder.decode(buffer.toByteBuffer).toString))
    } catch {
      case (_: MalformedInputException | _: UnmappableCharacterException) =>
        \/.left(s"${charset.displayName} cannot decode string from '0x${buffer.toByteVector.toHex}'")
    }
  }

  override def toString = charset.displayName
}
