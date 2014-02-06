package scodec
package codecs

import scalaz.\/-


class IgnoreCodec(bits: Int) extends Codec[Unit] {

  override def encode(unit: Unit) =
    \/-(BitVector.low(bits))

  override def decode(buffer: BitVector) =
    buffer.consume(bits) { _ => \/-(()) }

  override def toString = s"ignore($bits bits)"
}
