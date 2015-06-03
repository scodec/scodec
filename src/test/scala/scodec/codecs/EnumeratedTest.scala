package scodec.codecs

import org.scalacheck.{Gen, Arbitrary}
import scodec.bits._
import scodec.CodecSuite

class EnumeratedTest extends CodecSuite {

  object SIPrefix extends Enumeration {
    type SIPrefix = Value
    val DEKA = Value(10)
    val HECTO = Value(100)
    val KILO = Value(1000)
    val MEGA = Value(1000000)
    val GIGA = Value(1000000000)
  }

  "enumerated codec" should {

    val codec = enumerated(int32, SIPrefix)
    implicit def generator = Arbitrary(Gen.oneOf(SIPrefix.values.toSeq))

    "roundtrip" in {
      forAll { (v: SIPrefix.Value) => roundtrip(codec, v) }
    }

    "roundtrip with combinators" in {
      forAll { (i: Int, v: SIPrefix.Value) => roundtrip(int32 ~ codec, i ~ v) }
    }

    "fail for an invalid id" in {
      codec.decode(hex"00000001".bits).isFailure should equal (true)
    }
  }
}
