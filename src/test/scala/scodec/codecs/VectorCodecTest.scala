package scodec
package codecs

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import scalaz.{\/, \/-, -\/}
import scodec.bits._
import scodec.codecs._

class VectorCodecTest extends CodecSuite {

  "the vector codec" should {
    "be performant" in {
      val codec = vector(int32).complete
      val trials = 10
      val sizes = List(10, 100, 1000, 10000, 100000)
      val results = (1 to trials).map { trial =>
        sizes map { size =>
          val vec = definedSamples(Gen.listOfN(size, arbitrary[Int]).map { _.toVector }).head
          val (encoded, encodeTime) = time { codec.encodeValid(vec) }
          //info(s"$trial - encoding $size took $encodeTime")
          val (decoded, decodeTime) = time { codec.decodeValidValue(encoded) }
          //info(s"$trial - decoding $size took $decodeTime")
          decoded shouldBe vec
          encodeTime + decodeTime
        }
      }.drop(1) // drop first iteration to allow for JIT
      val avgs = results.reduceLeft((x, y) => (x zip y) map { case (a, b) => a + b }).map { _ / results.size }
      info("Roundtrip averages:")
      (sizes zip avgs).foreach { case (size, avg) => info(s"  $size - $avg") }
    }
  }

  "the vectorN codec" should {

    "limit decoding to the specified number of records" in {
      val codec = vectorN(provide(10), uint8)
      val buffer = BitVector.low(8 * 100)
      codec.decode(buffer) shouldBe \/.right((BitVector.low(8 * 90), Vector.fill(10)(0)))
    }

    "support encoding size before vector contents" in {
      val codec = vectorN(int32, uint8)
      codec.encode((1 to 10).toVector) shouldBe \/.right(hex"0000000a0102030405060708090a".bits)
    }

    "support not encoding size before vector contents" in {
      val codec = vectorN(provide(10), uint8)
      codec.encode((1 to 10).toVector) shouldBe \/.right(hex"102030405060708090a".bits)
    }
  }
}
