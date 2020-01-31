package scodec

import scodec.bits._
import scodec.codecs._
import shapeless._
import shapeless.union._
import shapeless.syntax.singleton._

class DerivedCodecTest extends CodecSuite {

  sealed trait Parent
  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent

  case class Qux(bar: Bar)
  case class Quy(x: Int, bar: Bar)
  case class Quz(x: Int, y: String, bars: Vector[Bar])

  case class Point(x: Int, y: Int, z: Int)
  case class Line(start: Point, end: Point)
  case class Arrangement(lines: Vector[Line])
  case class Woz(x: Int, y: String, pts: Vector[Point])

  case class Rec(x: Int, y: List[Rec])

  sealed abstract class Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  "automatic codec generation" should {
    "support automatic generation of HList codecs" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      Codec[Int :: Int :: String :: HNil]
        .encode(1 :: 2 :: "Hello" :: HNil)
        .require shouldBe hex"0102000548656c6c6f".bits
    }

    "support automatic generation of case class codecs" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      Codec[Foo].encode(Foo(1, 2, "Hello")).require shouldBe hex"0102000548656c6c6f".bits
    }

    "support automatic generation of nested case class codecs, where component codecs are derived as well" in {
      import implicits._
      Codec[Qux]
      Codec[Quy]
      Codec[Quz]
      Codec[Woz]

      val arr = Arrangement(
        Vector(Line(Point(0, 0, 0), Point(10, 10, 10)), Line(Point(0, 10, 1), Point(10, 0, 10)))
      )

      val arrBinary = Codec.encode(arr).require
      val decoded = Codec[Arrangement].decode(arrBinary).require.value
      decoded shouldBe arr
    }

    "include field names in case class codecs" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      Codec[Foo].encode(Foo(1, 256, "Hello")) shouldBe Attempt.failure(
        Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("y")
      )
    }

    "support automatic generation of coproduct codec builders" in {
      implicit val (u, s) = (constant(1), variableSizeBytes(uint16, utf8))
      type C = Unit :+: String :+: CNil
      val codec = Codec.coproduct[C].choice
      codec.encode(Coproduct[C]("Hello")).require shouldBe hex"000548656c6c6f".bits
      codec.encode(Coproduct[C](())).require shouldBe hex"01".bits
    }

    "support automatic generation of coproduct codec builders from union types" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      type U = Union.`'i -> Int, 's -> String`.T
      val codec = Codec.coproduct[U].discriminatedByIndex(uint8)
      codec.encode(Coproduct[U]('s ->> "Hello")).require shouldBe hex"01000548656c6c6f".bits
      codec.encode(Coproduct[U]('i ->> 256)) shouldBe Attempt.failure(
        Err("256 is greater than maximum value 255 for 8-bit unsigned integer").pushContext("i")
      )
    }

    "support automatic generation of coproduct codec builders from sealed trait and subclasses" in {
      implicit val (i, s) = (uint8, variableSizeBytes(uint16, utf8))
      val codec: Codec[Parent] = Codec.coproduct[Parent].discriminatedByIndex(uint8)
      codec.encode(Foo(1, 2, "Hello")).require shouldBe hex"010102000548656c6c6f".bits
      codec.encode(Bar(1, 2)).require shouldBe hex"000102".bits
    }

    "support recursive products" in {
      import codecs.implicits._
      val codec = Codec[Rec]
      roundtrip(codec, Rec(1, List(Rec(2, Nil))))
    }

    "support recursive ADTs" in {
      import codecs.implicits._
      implicit def d[A] = Discriminated[Tree[A], Boolean](bool)
      implicit def d0[A] = d[A].bind[Node[A]](false)
      implicit def d1[A] = d[A].bind[Leaf[A]](true)
      val codec = Codec[Tree[Int]]
      roundtrip(codec, Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))))
    }
  }
}
