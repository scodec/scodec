package scodec
package codecs

import java.util.UUID
import scodec.bits.BitVector

private[codecs] object UuidCodec extends Codec[UUID] {

  val codec = int64 ~ int64

  override def encode(u: UUID) =
    codec.encode((u.getMostSignificantBits, u.getLeastSignificantBits))

  override def decode(bits: BitVector) =
    codec.decode(bits) map { case (remaining, (m, l)) => (remaining, new UUID(m, l)) }

  override def toString = "uuid"
}
