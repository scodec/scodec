package scodec

import scalaz.syntax.id._

import java.nio.CharBuffer
import java.nio.charset.{Charset, CodingErrorAction}
import java.nio.charset.{MalformedInputException, UnmappableCharacterException}


class StringCodec(charset: Charset) extends Codec[String] {

  override def encode(str: String) = {
    val encoder = charset.newEncoder
    val buffer = CharBuffer.wrap(str)
    try BitVector(encoder.encode(buffer)).right
    catch {
      case (_: MalformedInputException | _: UnmappableCharacterException) =>
        s"${charset.displayName} cannot encode character '${buffer.charAt(0)}'".left
    }
  }

  override def decode(buffer: BitVector) = {
    val decoder = charset.newDecoder
    try {
      (BitVector.empty, decoder.decode(buffer.asByteBuffer).toString).right
    } catch {
      case (_: MalformedInputException | _: UnmappableCharacterException) =>
        s"${charset.displayName} cannot decode string from '${Bytes.toHexadecimal(buffer.asBytes)}'".left
    }
  }
}
