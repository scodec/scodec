package scodec
package codecs

import scodec.bits.crc
import scodec.bits.crc.crc32
import org.scalacheck.Prop.forAll

final class ChecksumAppendCodecTest extends ChecksumCodecTestBase {

  protected val checkSumString = new ChecksumAppendCodec(utf8, bitSize = 32, crc.crc32, true, identity)

  protected val checkSumLong = new ChecksumAppendCodec(int64, bitSize = 32, crc.crc32, true, identity)

  protected val checkSumLongFramed = new ChecksumAppendCodec(int64 :: int64, bitSize = 32, crc.crc32, true, _.dropRight(64))

  test("framing") {
    forAll { (body: (Long, Long)) =>
      val crcRegion = int64.encode(body._1).require
      val encodedCrc = checkSumLongFramed.encode(body).require.takeRight(32)
      crc32(crcRegion) == encodedCrc
    }
  }
}
