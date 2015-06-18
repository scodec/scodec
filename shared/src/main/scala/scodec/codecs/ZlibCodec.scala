package scodec
package codecs

import scodec.bits.BitVector

private[scodec] class ZlibCodec[A](codec: Codec[A], level: Int, strategy: Int, nowrap: Boolean, chunkSize: Int) extends Codec[A] {

  def sizeBound = SizeBound.unknown

  def encode(a: A) =
    codec.encode(a).map { b =>
      b.deflate(level, strategy, nowrap, chunkSize)
    }

  def decode(b: BitVector) =
    b.bytes.inflate(chunkSize).fold(
      e => Attempt.failure(Err(e.getMessage)),
      bb => codec.decode(bb.bits)
    )
}

