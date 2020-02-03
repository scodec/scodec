package scodec
package codecs

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import scodec.bits._

class VectorCodecTest extends CodecSuite {

  "the vector codec" should {
    "be performant" in {
      val codec = vector(int32).complete
      val trials = 10
      val sizes = List(10, 100, 1000, 10000)
      val results = (1 to trials)
        .map { trial =>
          sizes.map { size =>
            val vec = definedSamples(Gen.listOfN(size, arbitrary[Int]).map { _.toVector }).head
            val (encoded, encodeTime) = time { codec.encode(vec).require }
            //info(s"$trial - encoding $size took $encodeTime")
            val (decoded, decodeTime) = time { codec.decode(encoded).require.value }
            //info(s"$trial - decoding $size took $decodeTime")
            decoded shouldBe vec
            encodeTime + decodeTime
          }
        }
        .drop(1) // drop first iteration to allow for JIT
      val avgs = results.reduceLeft((x, y) => (x.zip(y)).map { case (a, b) => a + b }).map {
        _ / results.size.toLong
      }
      info("Roundtrip averages:")
      sizes.zip(avgs).foreach { case (size, avg) => info(s"  $size - $avg") }
    }

    "include index of problematic value when an error occurs during decoding" in {
      val codec = vector(
        uint8.narrow(
          x => if (x == 0) Attempt.failure(Err("zero disallowed")) else Attempt.successful(x),
          x => x
        )
      ).complete
      val result = codec.decode(hex"010200".bits)
      result shouldBe Attempt.failure(Err("zero disallowed").pushContext("2"))
    }

    "include index of problematic value when an error occurs during encoding" in {
      val codec = vector(uint8).complete
      val result = codec.encode(Vector(0, 1, -1))
      result shouldBe Attempt.failure(
        Err("-1 is less than minimum value 0 for 8-bit unsigned integer").pushContext("2")
      )
    }
  }

  "the vectorOfN codec" should {
    "limit decoding to the specified number of records" in {
      val codec = vectorOfN(provide(10), uint8)
      val buffer = BitVector.low(8 * 100)
      codec.decode(buffer) shouldBe Attempt.successful(
        DecodeResult(Vector.fill(10)(0), BitVector.low(8 * 90))
      )
    }

    "support encoding size before vector contents" in {
      val codec = vectorOfN(int32, uint8)
      codec.encode((1 to 10).toVector) shouldBe Attempt.successful(
        hex"0000000a0102030405060708090a".bits
      )
    }

    "support not encoding size before vector contents" in {
      val codec = vectorOfN(provide(10), uint8)
      codec.encode((1 to 10).toVector) shouldBe Attempt.successful(hex"102030405060708090a".bits)
    }

    "fails decoding if < N elements decoded" in {
      val codec = vectorOfN(provide(10), uint8)
      codec.decode(BitVector.low(8 * 5)) shouldBe Attempt.failure(
        Err("Insufficient number of elements: decoded 5 but should have decoded 10")
      )
    }
  }
}
