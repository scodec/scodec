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
  test("as on single codec") {
    roundtripAll(uint8.hlist.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
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
}
