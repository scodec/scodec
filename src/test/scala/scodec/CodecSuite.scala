package scodec

import scala.collection.GenTraversable

import scalaz.syntax.id._

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scodec.bits.BitVector

abstract class CodecSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  protected def roundtrip[A: Codec](a: A) {
    roundtrip(Codec[A], a)
  }

  protected def roundtrip[A](codec: Codec[A], a: A) {
    val encoded = codec.encode(a)
    encoded should be ('right)
    val (remainder, decoded) = codec.decode(encoded.toOption.get).toEither.right.get
    remainder shouldEqual BitVector.empty
    decoded shouldEqual  a
  }

  protected def roundtripAll[A](codec: Codec[A], as: GenTraversable[A]) {
    as foreach { a => roundtrip(codec, a) }
  }

}
