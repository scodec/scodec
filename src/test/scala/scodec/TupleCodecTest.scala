package scodec

import scalaz.\/-

import Codecs._


class TupleCodecTest extends CodecSuite {

  test("roundtrip") {
    roundtripAll(uint16 ~ uint16, Seq((0, 0), (0, 1), (65535, 42)))
    roundtripAll(uint16 ~ uint16 ~ uint32, Seq(((0, 0), 1L << 32 - 1), ((0, 1), 20L), ((65535, 42), 5L)))
  }

  test("~ extractor") {
    (uint8 ~ uint8 ~ uint8).decode(BitVector(24, 255, 14)) match {
      case \/-((rest, a ~ b ~ c)) =>
        rest should be ('empty)
        a should be (24)
        b should be (255)
        c should be (14)
    }

    Codec.decode(uint8 ~ uint8 ~ uint8, BitVector(1, 2, 3)) map { case a ~ b ~ c => a + b + c } should be (\/-(6))
  }
}
