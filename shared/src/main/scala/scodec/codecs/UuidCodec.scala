package scodec
package codecs

import java.util.UUID
import scodec.bits.BitVector

private[codecs] object UuidCodec extends Codec[UUID] {

  private val mkUUID: (Long, Long) => UUID = new UUID(_, _)

  override val sizeBound = int64.sizeBound * 2L

  override def encode(u: UUID) =
    Codec.encodeBoth(int64, int64)(u.getMostSignificantBits, u.getLeastSignificantBits)

  override def decode(bits: BitVector) =
    Codec.decodeBothCombine(int64, int64)(bits)(mkUUID)

  override def toString = "uuid"
}
