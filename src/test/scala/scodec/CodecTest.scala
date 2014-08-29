package scodec

import scalaz.\/

import scodec.bits._
import scodec.codecs._

class CodecTest extends CodecSuite {

  test("flatZip") {
    val codec = uint8 flatZip { n => fixedSizeBits(n, ascii) }
    roundtripAll(codec, Seq((0, ""), (8, "a"), (32, "test")))
  }

  test("complete") {
    val codec = codecs.bits(8)
    codec.decode(hex"00112233".toBitVector) shouldBe \/.right((hex"112233".toBitVector, hex"00".toBitVector))
    codec.complete.decode(hex"00112233".toBitVector) shouldBe \/.left("24 bits remaining: 0x112233")
    codec.complete.decode(BitVector.fill(2000)(false)) shouldBe \/.left("more than 512 bits remaining")
  }

  case class Bar(x: Int)

  test("as on an hlist codec of 1 element") {
    roundtripAll(uint8.hlist.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }

  test("as on a non-hlist codec") {
    roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
  }

  test("as in reverse direction") {
    import shapeless._
    uint8.hlist.as[Bar].as[Int :: HNil]
  }

  test("as on a non-hlist codec in reverse direction") {
    import shapeless._
    uint8.hlist.as[Bar].as[Int]
  }

  test("unit combinator") {
    import scalaz.std.AllInstances._
    val codec = uint8.unitM
    codec.encode(()) shouldBe \/.right(BitVector(0))
    codec.decode(BitVector(1)) shouldBe \/.right((BitVector.empty, ()))
    codec.decode(BitVector.empty) shouldBe 'left
    uint8.unit(255).encode(()) shouldBe \/.right(BitVector(0xff))
  }

  test("<~") {
    val codec = uint8 <~ uint8.unit(0)
    codec.encode(0xff) shouldBe \/.right(hex"ff00".bits)
  }

  test("literals used as unit codecs") {
    import scodec.codecs.literals._
    (1 ~> uint8).encode(2) shouldBe \/.right(hex"0102".bits)
    (1.toByte ~> uint8).encode(2) shouldBe \/.right(hex"0102".bits)
    (hex"11223344" ~> uint8).encode(2) shouldBe \/.right(hex"1122334402".bits)
    (hex"11223344".bits ~> uint8).encode(2) shouldBe \/.right(hex"1122334402".bits)
  }
}
