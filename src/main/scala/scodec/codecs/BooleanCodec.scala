package scodec
package codecs

import scalaz.\/-


object BooleanCodec extends Codec[Boolean] {

  override def encode(b: Boolean) =
    \/-(if (b) BitVector.high(1) else BitVector.low(1))

  override def decode(buffer: BitVector) =
    buffer.consume(1) { b => \/-(b.head) }

  override def toString = "bool"
}
