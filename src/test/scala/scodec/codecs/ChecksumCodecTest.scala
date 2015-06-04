package scodec.codecs

import scodec._
import scodec.bits._

/**
 * ChecksumCodecTest
 */
class ChecksumCodecTest extends CodecSuite {

  "checksummed codec" should {
    val codec = checksummed(ChecksumCodec.xor(1, int32, 4), variableSizeBytes(int32, utf8))

    "roundtrip" in {
      forAll { (s: String) => roundtrip(codec, s) }
    }

    "roundtrip using combinators" in {
      forAll { (n: Int, s: String) => roundtrip(int32 ~ codec, n ~ s) }
    }

    "append checksum on encode" in {
      codec.encode("hello world").require should equal(hex"0x0000000b68656c6c6f20776f726c642b".bits)
    }

    "verify (and remove) checksum on decode" in {
      codec.decode(hex"0x0000000b68656c6c6f20776f726c642b".bits).require.value should equal("hello world")
      codec.decode(hex"0x0000000b68656c6c6f20776f726c642b".bits).require.remainder should equal(BitVector.empty)
    }

    "fail decoding on checksum mismatch" in {
      codec.decode(hex"0x0000000b68656c6c6f20776f726c6400".bits) should equal(Attempt.failure(ChecksumCodec.Mismatch(hex"0x0000000b68656c6c6f20776f726c64".bits, hex"2b".bits, hex"00".bits)))
    }
  }
}