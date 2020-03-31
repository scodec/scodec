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

  test("derivation of tuple codecs") {
    given Codec[Int] = uint8
    given Codec[String] = variableSizeBytes(uint16, utf8)
    assertBitsEqual(
      Codec[(Int, Int, String)].encode(1, 2, "Hello").require,
      hex"0102000548656c6c6f".bits)
  }

  test("derivation of case class codecs") {
    assertBitsEqual(Codec[Foo].encode(Foo(1, 2, "Hello")).require, 0x00000001000000020000000548656c6c6f)
  }

  test("derivation of nested case class codecs, where component codecs are derived as well") {
    Codec[Qux]
    Codec[Quy]
    Codec[Quz]
    Codec[Woz]

    val arr = Arrangement(
      Vector(Line(Point(0, 0, 0), Point(10, 10, 10)), Line(Point(0, 10, 1), Point(10, 0, 10)))
    )

    val arrBinary = Codec[Arrangement].encode(arr).require
    val decoded = Codec[Arrangement].decode(arrBinary).require.value
    assertEquals(decoded, arr)
  }

  test("derivation of recursive products") {
    roundtrip(Codec[Rec], Rec(1, List(Rec(2, Nil))))
  }

  test("derivation of recursive ADTs") {
    roundtrip(Codec[Tree[Int]], Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))))
  }
}
