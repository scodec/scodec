package scodec.codecs

import scodec._
import scodec.bits._

class ChecksumCodecTest extends CodecSuite {

  "checksummed codec" should {
    def xor(length: Long) = (bits: BitVector) => bits.grouped(length).foldLeft(BitVector.low(length))(_ xor _)

    val codecSizeIncluded = checksummed(utf8_32, xor(8), peekVariableSizeBytes(int32) ~ bits)
    val codecSizeExcluded = checksummed(utf8, xor(8), variableSizeBytes(int32, bits) ~ bits)

    "roundtrip" in {
      forAll { (s: String) => roundtrip(codecSizeIncluded, s) }
      forAll { (s: String) => roundtrip(codecSizeExcluded, s) }
    }

    "roundtrip using combinators" in {
      forAll { (n: Int, s: String) => roundtrip(int32 ~ codecSizeIncluded, n ~ s) }
      forAll { (n: Int, s: String) => roundtrip(int32 ~ codecSizeExcluded, n ~ s) }
    }

    "append checksum on encode" in {
      codecSizeIncluded.encode("hello world").require shouldBe hex"0x0000000b68656c6c6f20776f726c642b".bits
    }

    "verify (and remove) checksum on decode" in {
      codecSizeIncluded.decode(hex"0x0000000b68656c6c6f20776f726c642b".bits).require.value shouldBe "hello world"
      codecSizeIncluded.decode(hex"0x0000000b68656c6c6f20776f726c642b".bits).require.remainder shouldBe BitVector.empty
    }

    "fail decoding on checksum mismatch" in {
      codecSizeIncluded.decode(hex"0x0000000b68656c6c6f20776f726c6400".bits) shouldBe Attempt.failure(ChecksumMismatch(hex"0x0000000b68656c6c6f20776f726c64".bits, hex"2b".bits, hex"00".bits))
    }

    "support putting the checksum before the data" in {
      val crc32c = crc(hex"1edc6f41".bits, hex"ffffffff".bits, true, true, hex"ffffffff".bits)
      def swap(c: Codec[(BitVector, BitVector)]): Codec[(BitVector, BitVector)] =
        c.xmap(_.swap, _.swap)
      val codec = checksummed(utf8_32, crc32c, swap(bits(32) ~ bits))
      codec.encode("hello world").require shouldBe hex"edbbac630000000b68656c6c6f20776f726c64".bits
    }
  }
}
