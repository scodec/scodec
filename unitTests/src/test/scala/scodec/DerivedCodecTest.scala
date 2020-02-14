package scodec

import scodec.bits._
import scodec.codecs._

class DerivedCodecTest extends CodecSuite {

  sealed trait Parent derives Codec
  case class Foo(x: Int, y: Int, s: String) extends Parent derives Codec
  case class Bar(x: Int, y: Int) extends Parent derives Codec

  case class Qux(bar: Bar) derives Codec
  case class Quy(x: Int, bar: Bar) derives Codec
  case class Quz(x: Int, y: String, bars: Vector[Bar]) derives Codec

  case class Point(x: Int, y: Int, z: Int) derives Codec
  case class Line(start: Point, end: Point) derives Codec
  case class Arrangement(lines: Vector[Line]) derives Codec
  case class Woz(x: Int, y: String, pts: Vector[Point]) derives Codec

  case class Rec(x: Int, y: List[Rec]) derives Codec

  sealed abstract class Tree[A] derives Codec
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  "automatic codec generation" should {
    "support automatic generation of HList codecs" in {
      given Codec[Int] = uint8
      given Codec[String] = variableSizeBytes(uint16, utf8)
      assertBitsEqual(
        summon[Codec[(Int, Int, String)]].encode(1, 2, "Hello").require,
        hex"0102000548656c6c6f".bits)
    }

    "support automatic generation of case class codecs" in {
      assertBitsEqual(summon[Codec[Foo]].encode(Foo(1, 2, "Hello")).require, 0x00000001000000020000000548656c6c6f)
    }

    "support automatic generation of nested case class codecs, where component codecs are derived as well" in {
      summon[Codec[Qux]]
      summon[Codec[Quy]]
      summon[Codec[Quz]]
      summon[Codec[Woz]]

      val arr = Arrangement(
        Vector(Line(Point(0, 0, 0), Point(10, 10, 10)), Line(Point(0, 10, 1), Point(10, 0, 10)))
      )

      val arrBinary = summon[Codec[Arrangement]].encode(arr).require
      val decoded = summon[Codec[Arrangement]].decode(arrBinary).require.value
      assert(decoded == arr)
    }

    // "support automatic generation of coproduct codec builders" in {
    //   implicit val (u, s) = (constant(1), variableSizeBytes(uint16, utf8))
    //   type C = Unit :+: String :+: CNil
    //   val codec = Codec.coproduct[C].choice
    //   codec.encode(Coproduct[C]("Hello")).require shouldBe hex"000548656c6c6f".bits
    //   codec.encode(Coproduct[C](())).require shouldBe hex"01".bits
    // }

    // "support automatic generation of coproduct codec builders from union types" in {
    //   implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
    //   type U = Union.`'i -> Int, 's -> String`.T
    //   val codec = Codec.coproduct[U].discriminatedByIndex(uint8)
    //   codec.encode(Coproduct[U]('s ->> "Hello")).require shouldBe hex"01000548656c6c6f".bits
    //   codec.encode(Coproduct[U]('i ->> 256)) shouldBe Attempt.failure(
    //     Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("i")
    //   )
    // }

    // "support automatic generation of coproduct codec builders from sealed trait and subclasses" in {
    //   implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
    //   val codec: Codec[Parent] = Codec.coproduct[Parent].discriminatedByIndex(uint8)
    //   codec.encode(Foo(1, 2, "Hello")).require shouldBe hex"010102000548656c6c6f".bits
    //   codec.encode(Bar(1, 2)).require shouldBe hex"000102".bits
    // }

    "support recursive products" in {
      roundtrip(summon[Codec[Rec]], Rec(1, List(Rec(2, Nil))))
    }

    "support recursive ADTs" in {
      roundtrip(summon[Codec[Tree[Int]]], Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))))
    }
  }
}
