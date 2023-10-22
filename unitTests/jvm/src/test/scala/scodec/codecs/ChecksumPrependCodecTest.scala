package scodec
package codecs

import scodec.bits.crc
import scodec.bits.crc.crc32
import org.scalacheck.Prop.forAll

final class ChecksumPrependCodecTest extends ChecksumCodecTestBase {

  protected val checkSumString =
    new ChecksumPrependCodec(utf8, bitSize = 32, crc.crc32, true, identity)

  protected val checkSumLong =
    new ChecksumPrependCodec(int64, bitSize = 32, crc.crc32, true, identity)

  protected val checkSumLongFramed =
    new ChecksumPrependCodec(int64 :: int64, bitSize = 32, crc.crc32, true, _.drop(64))

  test("framing") {
    forAll { (body: (Long, Long)) =>
      val crcRegion = int64.encode(body._2).require
      val encodedCrc = checkSumLongFramed.encode(body).require.take(32)
      crc32(crcRegion) == encodedCrc
    }
  }
}
