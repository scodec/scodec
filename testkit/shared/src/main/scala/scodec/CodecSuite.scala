package scodec

import scala.concurrent.duration._

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import org.scalactic.source.Position
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scodec.bits.BitVector

abstract class CodecSuite extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100, workers = 4)

  protected def roundtrip[A](a: A)(implicit c: Codec[A]): Unit =
    roundtrip(c, a)

  protected def roundtrip[A](codec: Codec[A], value: A): Unit = {
    val encoded = codec.encode(value)
    assert(encoded.isSuccessful)
    val Attempt.Successful(DecodeResult(decoded, remainder)) = codec.decode(encoded.require)
    assertBitsEqual(remainder, BitVector.empty)
    assert(decoded == value)
    ()
  }

  protected def roundtripAll[A](codec: Codec[A], as: collection.Iterable[A]): Unit =
    as.foreach(a => roundtrip(codec, a))

  protected def encodeError[A](codec: Codec[A], a: A, err: Err) = {
    val encoded = codec.encode(a)
    assert(encoded == Attempt.Failure(err))
  }

  protected def shouldDecodeFullyTo[A](codec: Codec[A], buf: BitVector, expected: A) = {
    val Attempt.Successful(DecodeResult(actual, rest)) = codec.decode(buf)
    assertBitsEqual(rest, BitVector.empty)
    assert(actual == expected)
  }

  protected def time[A](f: => A): (A, FiniteDuration) = {
    val start = System.nanoTime
    val result = f
    val elapsed = (System.nanoTime - start).nanos
    (result, elapsed)
  }

  protected def samples[A](gen: Gen[A]): LazyList[Option[A]] =
    LazyList.continually(gen.sample)

  protected def definedSamples[A](gen: Gen[A]): LazyList[A] =
    samples(gen).flatten

  implicit def arbBitVector: Arbitrary[BitVector] =
    Arbitrary(arbitrary[Array[Byte]].map(BitVector.apply))

  protected def assertBitsEqual(actual: BitVector, expected: BitVector)(using p: Position) = assert(actual == expected)
}
