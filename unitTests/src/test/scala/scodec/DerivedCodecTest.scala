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
    "support automatic generation of tuple codecs" in {
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

    "support recursive products" in {
      roundtrip(summon[Codec[Rec]], Rec(1, List(Rec(2, Nil))))
    }

    "support recursive ADTs" in {
      roundtrip(summon[Codec[Tree[Int]]], Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))))
    }
  }
}
