package scodec.codecs

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
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

  val codec = enumerated(int32, SIPrefix)
  implicit def generator: Arbitrary[SIPrefix.Value] = Arbitrary(Gen.oneOf(SIPrefix.values.toSeq))

  property("roundtrip") {
    forAll((v: SIPrefix.Value) => roundtrip(codec, v))
  }

  property("roundtrip with combinators") {
    forAll((i: Int, v: SIPrefix.Value) => roundtrip(int32 :: codec, (i, v)))
  }

  test("fail for an invalid id") {
    assert(codec.decode(hex"000000FF".bits).isFailure)
  }
}
