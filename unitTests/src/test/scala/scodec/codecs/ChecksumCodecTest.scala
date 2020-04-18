package scodec.codecs

import scodec._
import scodec.bits._

import org.scalacheck.Prop.forAll

class ChecksumCodecTest extends CodecSuite {

  def xor(length: Long) =
    (bits: BitVector) => bits.grouped(length).foldLeft(BitVector.low(length))(_.xor(_))

  val codecSizeIncluded = checksummed(utf8_32, xor(8), peekVariableSizeBytes(int32) :: codecs.bits)
  val codecSizeExcluded =
    checksummed(utf8, xor(8), variableSizeBytes(int32, codecs.bits) :: codecs.bits)

  property("roundtrip (1)") {
    forAll((s: String) => roundtrip(codecSizeIncluded, s))
  }
  property("roundtrip (2)") {
    forAll((s: String) => roundtrip(codecSizeExcluded, s))
  }

  property("roundtrip using combinators (1)") {
    forAll((n: Int, s: String) => roundtrip(int32 :: codecSizeIncluded, (n, s)))
  }
  property("roundtrip using combinators (2)") {
    forAll((n: Int, s: String) => roundtrip(int32 :: codecSizeExcluded, (n, s)))
  }

  test("append checksum on encode") {
    assertEquals(codecSizeIncluded
      .encode("hello world")
      .require, hex"0x0000000b68656c6c6f20776f726c642b".bits)
  }

  test("verify (and remove) checksum on decode") {
    assertEquals(codecSizeIncluded
      .decode(hex"0x0000000b68656c6c6f20776f726c642b".bits)
      .require
      .value, "hello world")
    assertEquals(codecSizeIncluded
      .decode(hex"0x0000000b68656c6c6f20776f726c642b".bits)
      .require
      .remainder, BitVector.empty)
  }

  test("fail decoding on checksum mismatch") {
    assertEquals(codecSizeIncluded.decode(hex"0x0000000b68656c6c6f20776f726c6400".bits), Attempt
      .failure(
        ChecksumMismatch(hex"0x0000000b68656c6c6f20776f726c64".bits, hex"2b".bits, hex"00".bits)
      ))
  }

  test("support putting the checksum before the data") {
    val crc32c = crc(hex"1edc6f41".bits, hex"ffffffff".bits, true, true, hex"ffffffff".bits)
    def swap(c: Codec[(BitVector, BitVector)]): Codec[(BitVector, BitVector)] =
      c.xmap(_.swap, _.swap)
    val codec = checksummed(utf8_32, crc32c, swap(codecs.bits(32) :: codecs.bits))
    assertEquals(codec.encode("hello world").require, hex"edbbac630000000b68656c6c6f20776f726c64".bits)
  }
}
