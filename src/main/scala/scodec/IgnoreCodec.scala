package scodec

import scalaz.\/-


class IgnoreCodec(bits: Int) extends Codec[Unit] {

  override def encode(unit: Unit) =
    \/-(BitVector.low(bits))

  override def decode(buffer: BitVector) =
    buffer.consume(bits) { _ => \/-(()) }
}
