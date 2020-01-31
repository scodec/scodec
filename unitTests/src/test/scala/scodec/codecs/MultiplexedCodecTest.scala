package scodec.codecs

import org.scalacheck.{Arbitrary, Gen}
import scodec._
import scodec.bits._

class MultiplexedCodecTest extends CodecSuite {

  "listDelimited codec" should {
    val codec = listDelimited(BitVector(' '), ascii)
    val bits = ascii.encode("i am delimited").require
    val list = List("i", "am", "delimited")
    "encode a list" in {
      codec.encode(list).require shouldBe bits
    }
    "decode to a list" in {
      codec.decode(bits).require.value shouldBe list
      codec.decode(bits).require.remainder shouldBe BitVector.empty
    }
    "handle when delimiter is far from start" in {
      val data =
        hex"22993a01353a90223a093af20129260750062759082049964318053a2016283a203a003af230493af9783a003a22".bits
      listDelimited(hex"3a".bits, bytes).decode(data).require.remainder shouldBe BitVector.empty
    }
  }

  "vectorMultiplexed codec" should {
    "be performant" in {
      val delimiter = BitVector.empty
      // a simplistic example to test performance of mux/deMux
      // a realistic example would have a more complex deMux (potentially resulting in exponential time complexities)
      val codec =
        vectorMultiplexed(_ ++ delimiter ++ _, bits => (bits.compact, BitVector.empty), int32)
      val trials = 10
      val sizes = List(10, 100, 1000, 10000)
      val results = (1 to trials)
        .map { trial =>
          sizes.map { size =>
            val vec = definedSamples(Gen.listOfN(size, Arbitrary.arbitrary[Int]).map {
              _.toVector
            }).head
            val (encoded, encodeTime) = time {
              codec.encode(vec).require
            }
            // info(s"$trial - encoding $size took $encodeTime")
            val (decoded, decodeTime) = time {
              codec.decode(encoded).require.value
            }
            // info(s"$trial - decoding $size took $decodeTime")
            decoded shouldBe vec
            encodeTime + decodeTime
          }
        }
        .drop(1) // drop first iteration to allow for JIT
      val averages = results.reduceLeft((x, y) => (x.zip(y)).map { case (z, b) => z + b }).map {
        _ / results.size.toLong
      }
      info("Roundtrip averages:")
      sizes.zip(averages).foreach { case (s, average) => info(s"  $s - $average") }
    }
  }
}
