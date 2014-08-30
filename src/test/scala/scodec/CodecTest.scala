package scodec

import scalaz.\/

import scodec.bits._
import scodec.codecs._

class CodecTest extends CodecSuite {
  case class Bar(x: Int)

  "all codecs" should {

    "support flatZip" in {
      val codec = uint8 flatZip { n => fixedSizeBits(n, ascii) }
      roundtripAll(codec, Seq((0, ""), (8, "a"), (32, "test")))
    }

    "support complete combinator" in {
      val codec = codecs.bits(8)
      codec.decode(hex"00112233".toBitVector) shouldBe \/.right((hex"112233".toBitVector, hex"00".toBitVector))
      codec.complete.decode(hex"00112233".toBitVector) shouldBe \/.left("24 bits remaining: 0x112233")
      codec.complete.decode(BitVector.fill(2000)(false)) shouldBe \/.left("more than 512 bits remaining")
    }

    "support as method for converting to a new codec using implicit isomorphism," which {

      "works with HList codecs of 1 element" in {
        roundtripAll(uint8.hlist.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
      }

      "works with non-HList codecs" in {
        roundtripAll(uint8.as[Bar], Seq(Bar(0), Bar(1), Bar(255)))
      }

      "supports destructuring case classes in to HLists" in {
        import shapeless._
        uint8.hlist.as[Bar].as[Int :: HNil]
      }

      "supports destructuring singleton case classes in to values" in {
        import shapeless._
        uint8.hlist.as[Bar].as[Int]
      }
    }

    "support the unit combinator" in {
      import scalaz.std.AllInstances._
      val codec = uint8.unitM
      codec.encode(()) shouldBe \/.right(BitVector(0))
      codec.decode(BitVector(1)) shouldBe \/.right((BitVector.empty, ()))
      codec.decode(BitVector.empty) shouldBe 'left
      uint8.unit(255).encode(()) shouldBe \/.right(BitVector(0xff))
    }

    "support dropRight combinator" in {
      val codec = uint8 <~ uint8.unit(0)
      codec.encode(0xff) shouldBe \/.right(hex"ff00".bits)
    }
  }

  "literal values" should {
    "be usable as constant codecs" in {
      import scodec.codecs.literals._
      (1 ~> uint8).encode(2) shouldBe \/.right(hex"0102".bits)
      (1.toByte ~> uint8).encode(2) shouldBe \/.right(hex"0102".bits)
      (hex"11223344" ~> uint8).encode(2) shouldBe \/.right(hex"1122334402".bits)
      (hex"11223344".bits ~> uint8).encode(2) shouldBe \/.right(hex"1122334402".bits)
    }
  }
}
