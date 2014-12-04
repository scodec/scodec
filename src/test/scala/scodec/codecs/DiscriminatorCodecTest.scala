package scodec
package codecs

import scalaz.syntax.either._

class DiscriminatorCodecTest extends CodecSuite {

  "the discriminator combinators" should {

    "support building a codec using typecases" in {
      val codec =
        discriminated[AnyVal].by(uint8)
        .typecase(0, int32)
        .typecase(1, bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      roundtrip(codec, 1)
      roundtrip(codec, Int.MaxValue)
    }

    "support building a codec using partial functions and subtyping" in {
      val codec =
        discriminated[AnyVal].by(uint8)
        .\ (0) { case i: Int => i } (int32)
        .\ (1) { case b: Boolean => b } (bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      roundtrip(codec, 1)
      roundtrip(codec, Int.MaxValue)
    }

    "support building a codec using A => Option[B] and subtyping" in {
      val codec =
        discriminated[AnyVal].by(uint8)
        ./ (0) { v => v match { case i: Int => Some(i); case _ => None }} (int32)
        ./ (1) { v => v match { case b: Boolean => Some(b); case _ => None }} (bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      roundtrip(codec, 1)
      roundtrip(codec, Int.MaxValue)
    }

    "support building a codec for an enumeration" in {
      sealed trait Direction
      case object North extends Direction
      case object South extends Direction
      case object East extends Direction
      case object West extends Direction

      val codec = mappedEnum(uint8, North -> 1, South -> 2, East -> 3, West -> 4)

      roundtrip(codec, North)
      roundtrip(codec, South)
      roundtrip(codec, East)
      roundtrip(codec, West)
    }

    "support building a codec for an enumeration with preserved reserved values" in {
      trait Color
      case object Red extends Color
      case object Green extends Color
      case object Blue extends Color
      case class Reserved(value: Int) extends Color

      val nonReserved: Codec[Color] = mappedEnum(uint8, Red -> 1, Green -> 2, Blue -> 3)
      val reserved: Codec[Reserved] = uint8.pxmap(Reserved.apply, Reserved.unapply)
      val codec: Codec[Color] = choice(nonReserved, reserved.upcast[Color])

      roundtrip(codec, Red)
      roundtrip(codec, Green)
      roundtrip(codec, Blue)
      roundtrip(codec, Reserved(255))
      roundtrip(codec, Reserved(4))
    }

    "support building a codec for an enumeration with preserved reserved values, and reserved values are not in the type hierarchy" in {
      import scalaz.{ \/, \/-, -\/ }
      import \/.{ left, right }

      trait Color
      case object Red extends Color
      case object Green extends Color
      case object Blue extends Color

      case class Reserved(value: Int)

      val nonReserved: Codec[Color] = mappedEnum(uint8, Red -> 1, Green -> 2, Blue -> 3)
      val reserved: Codec[Reserved] = uint8.pxmap(Reserved.apply, Reserved.unapply)
      val codec: Codec[Reserved \/ Color] = choice(
        nonReserved.xmap[\/-[Color]](c => \/-(c), _.b).upcast[Reserved \/ Color],
        reserved.xmap[-\/[Reserved]](r => -\/(r), _.a).upcast[Reserved \/ Color]
      )

      roundtrip(codec, right(Red))
      roundtrip(codec, right(Green))
      roundtrip(codec, right(Blue))
      roundtrip(codec, left(Reserved(255)))
      roundtrip(codec, left(Reserved(4)))
    }


    "support building a codec for an ADT" in {
      sealed trait Direction
      case object Stay extends Direction
      case class Go(units: Int) extends Direction

      val stayCodec = provide(Stay)
      val goCodec = int32.pxmap[Go](Go.apply, Go.unapply)

      val codec =
        discriminated[Direction].by(uint8).
          typecase(0, stayCodec).
          typecase(1, goCodec)

      roundtrip(codec, Stay)
      roundtrip(codec, Go(42))
    }

    "support building a codec for recusive ADTs - e.g., trees" in {
      sealed trait Tree
      case class Node(l: Tree, r: Tree) extends Tree
      case class Leaf(n: Int) extends Tree

      def treeCodec: Codec[Tree] = lazily {
        discriminated[Tree].by(bool)
        .| (false) { case l @ Leaf(n) => n } (Leaf.apply) (int32)
        .| (true)  { case n @ Node(l, r) => (l, r) } { case (l, r) => Node(l, r) } (treeCodec ~ treeCodec)
      }

      roundtrip(treeCodec, Leaf(42))
      roundtrip(treeCodec, Node(Leaf(42), Node(Leaf(1), Leaf(2))))
    }
  }
}
