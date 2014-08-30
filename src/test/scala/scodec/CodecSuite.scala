package scodec

import scala.collection.GenTraversable

import scalaz.\/-
import scalaz.syntax.either._

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

  protected def shouldDecodeFullyTo[A](codec: Codec[A], buf: BitVector, expected: A): Unit = {
    val \/-((rest, actual)) = codec decode buf
    rest shouldBe BitVector.empty
    actual shouldBe expected
  }
}
