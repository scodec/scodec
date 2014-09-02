package scodec
package codecs

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import scalaz.{\/-, -\/}
import scodec.bits.BitVector
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
}
