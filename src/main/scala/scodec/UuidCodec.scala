package scodec

import java.util.UUID


object UuidCodec extends Codec[UUID] with TupleCodecSyntax {

  val codec = Codecs.int64 ~ Codecs.int64

  override def encode(u: UUID) =
    codec.encode((u.getMostSignificantBits, u.getLeastSignificantBits))

  override def decode(bits: BitVector) =
    codec.decode(bits) map { case (remaining, (m, l)) => (remaining, new UUID(m, l)) }
}
