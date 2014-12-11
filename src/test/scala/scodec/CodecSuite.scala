package scodec

import scala.collection.GenTraversable
import scala.concurrent.duration._

import scalaz.{-\/, \/-}
import scalaz.syntax.either._

import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scodec.bits.BitVector

abstract class CodecSuite extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  protected def roundtrip[A: Codec](a: A) {
    roundtrip(Codec[A], a)
  }

  protected def roundtrip[A](codec: Codec[A], a: A) {
    val encoded = codec.encode(a)
    encoded should be ('right)
    val \/-((remainder, decoded)) = codec.decode(encoded.toOption.get)
    remainder shouldEqual BitVector.empty
    decoded shouldEqual a
  }

  protected def roundtripAll[A](codec: Codec[A], as: GenTraversable[A]) {
    as foreach { a => roundtrip(codec, a) }
  }

  protected def encodeError[A](codec: Codec[A], a: A, err: Err) {
    val encoded = codec.encode(a)
    encoded should be ('left)
    val -\/(error) = encoded
    error shouldBe err
  }

  protected def shouldDecodeFullyTo[A](codec: Codec[A], buf: BitVector, expected: A): Unit = {
    val \/-((rest, actual)) = codec decode buf
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
    samples(gen).flatMap { x => x }
}
