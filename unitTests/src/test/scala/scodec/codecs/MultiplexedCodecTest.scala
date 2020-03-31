package scodec.codecs

import org.scalacheck.{Arbitrary, Gen}
import scodec._
import scodec.bits._

class MultiplexedCodecTest extends CodecSuite {

  val codec = listDelimited(BitVector(' '), ascii)
  val bits = ascii.encode("i am delimited").require
  val list = List("i", "am", "delimited")

  test("listDelimited - encode") {
    assertEquals(codec.encode(list).require, bits)
  }

  test("listDelimited - decode") {
    assertEquals(codec.decode(bits).require.value, list)
    assertEquals(codec.decode(bits).require.remainder, BitVector.empty)
  }

  test("listDelimited - handle when delimiter is far from start") {
    val data =
      hex"22993a01353a90223a093af20129260750062759082049964318053a2016283a203a003af230493af9783a003a22".bits
    assertEquals(listDelimited(hex"3a".bits, bytes).decode(data).require.remainder, BitVector.empty)
  }

  test("vectorMultiplexed - performance") {
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
          // println(s"$trial - encoding $size took $encodeTime")
          val (decoded, decodeTime) = time {
            codec.decode(encoded).require.value
          }
          // println(s"$trial - decoding $size took $decodeTime")
          assertEquals(decoded, vec)
          encodeTime + decodeTime
        }
      }
      .drop(1) // drop first iteration to allow for JIT
    val averages = results.reduceLeft((x, y) => (x.zip(y)).map { case (z, b) => z + b }).map {
      _ / results.size.toLong
    }
    println("Roundtrip averages:")
    sizes.zip(averages).foreach { case (s, average) => println(s"  $s - $average") }
  }
}
