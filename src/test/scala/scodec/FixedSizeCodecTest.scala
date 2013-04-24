package scodec

import scalaz.{\/-, -\/}

import Codecs._


class FixedSizeCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtrip(fixedSizeBits(32, ascii), "test")
  }

  test("range checks") {
    val encoded = ascii.encode("test").toOption.get
    fixedSizeBits(32, ascii).decode(encoded ++ BitVector.low(48)) shouldBe \/-((BitVector.low(48), "test"))
    fixedSizeBits(24, ascii).encode("test") shouldBe -\/("Encoding [test] requires 32 bits but field is fixed size of 24 bits")
  }
}
