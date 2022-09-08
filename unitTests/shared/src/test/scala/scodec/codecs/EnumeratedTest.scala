package scodec.codecs

import org.scalacheck.{Arbitrary, Gen}
import scodec.bits._
import scodec.CodecSuite

class EnumeratedTest extends CodecSuite {

  object SIPrefix extends Enumeration {
    type SIPrefix = Value
    val DEKA = Value
    val HECTO = Value
    val KILO = Value
    val MEGA = Value
    val GIGA = Value
  }

  "enumerated codec" should {
    val codec = enumerated(int32, SIPrefix)
    implicit def generator = Arbitrary(Gen.oneOf(SIPrefix.values.toSeq))

    "roundtrip" in {
      forAll { (v: SIPrefix.Value) =>
        roundtrip(codec, v)
      }
    }

    "roundtrip with combinators" in {
      forAll { (i: Int, v: SIPrefix.Value) =>
        roundtrip(int32 ~ codec, i ~ v)
      }
    }

    "fail for an invalid id" in {
      codec.decode(hex"000000FF".bits).isFailure should equal(true)
    }
  }
}
