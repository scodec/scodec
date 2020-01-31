package scodec

import scala.concurrent.duration._

import shapeless.Lazy

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import org.scalatest.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scodec.bits.BitVector

abstract class CodecSuite extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100, workers = 4)

  protected def roundtrip[A](a: A)(implicit c: Lazy[Codec[A]]): Unit =
    roundtrip(c.value, a)

  protected def roundtrip[A](codec: Codec[A], value: A): Unit = {
    val encoded = codec.encode(value)
    encoded.isSuccessful shouldBe true
    val Attempt.Successful(DecodeResult(decoded, remainder)) = codec.decode(encoded.require)
    remainder shouldEqual BitVector.empty
    decoded shouldEqual value
    ()
  }

  protected def roundtripAll[A](codec: Codec[A], as: collection.Iterable[A]): Unit =
    as.foreach { a =>
      roundtrip(codec, a)
    }

  protected def encodeError[A](codec: Codec[A], a: A, err: Err) = {
    val encoded = codec.encode(a)
    encoded shouldBe Attempt.Failure(err)
  }

  protected def shouldDecodeFullyTo[A](codec: Codec[A], buf: BitVector, expected: A) = {
    val Attempt.Successful(DecodeResult(actual, rest)) = codec.decode(buf)
    rest shouldBe BitVector.empty
    actual shouldBe expected
  }

  protected def time[A](f: => A): (A, FiniteDuration) = {
    val start = System.nanoTime
    val result = f
    val elapsed = (System.nanoTime - start).nanos
    (result, elapsed)
  }

  protected def samples[A](gen: Gen[A]): Stream[Option[A]] =
    Stream.continually(gen.sample)

  protected def definedSamples[A](gen: Gen[A]): Stream[A] =
    samples(gen).flatMap { x =>
      x
    }

  implicit def arbBitVector: Arbitrary[BitVector] =
    Arbitrary(arbitrary[Array[Byte]].map(BitVector.apply))
}
