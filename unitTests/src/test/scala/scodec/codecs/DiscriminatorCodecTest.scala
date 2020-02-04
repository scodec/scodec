package scodec
package codecs

class DiscriminatorCodecTest extends CodecSuite {

  "the discriminator combinators" should {

    "support building a codec using typecases" in {
      val codec =
        discriminated[AnyVal]
          .by(uint8)
          .typecase(0, int32)
          .typecase(1, bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      roundtrip(codec, 1)
      roundtrip(codec, Int.MaxValue)
      codec.sizeBound shouldBe SizeBound.bounded(9, 40)
    }

    "support building a codec using partial functions and subtyping" in {
      val codec =
        discriminated[Any]
          .by(uint8)
          .subcaseP[Int](0) { case i: Int => i }(int32)
          .subcaseP[Boolean](1) { case b: Boolean => b }(bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      roundtrip(codec, 1)
      roundtrip(codec, Int.MaxValue)
    }

    "support building a codec using A => Option[B] and subtyping" in {
      val codec =
        discriminated[Any]
          .by(uint8)
          .subcaseO[Int](0) { v =>
            v match { case i: Int => Some(i); case _ => None }
          }(int32)
          .subcaseO[Boolean](1) { v =>
            v match { case b: Boolean => Some(b); case _ => None }
          }(bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      roundtrip(codec, 1)
      roundtrip(codec, Int.MaxValue)
    }

    "support building a codec for an enumeration" in {
      enum Direction { case North, South, East, West }

      val codec = mappedEnum(uint8, Direction.North -> 1, Direction.South -> 2, Direction.East -> 3, Direction.West -> 4)

      roundtrip(codec, Direction.North)
      roundtrip(codec, Direction.South)
      roundtrip(codec, Direction.East)
      roundtrip(codec, Direction.West)
    }

    "support building a codec for an enumeration with preserved reserved values" in {
      enum Color {
        case Red, Green, Blue
        case Reserved(value: Int)
      }

      val nonReserved: Codec[Color] = mappedEnum(uint8, Color.Red -> 1, Color.Green -> 2, Color.Blue -> 3)
      val reserved: Codec[Color.Reserved] = uint8.as[Color.Reserved]
      val codec: Codec[Color] = choice(nonReserved, reserved.upcast[Color])

      roundtrip(codec, Color.Red)
      roundtrip(codec, Color.Green)
      roundtrip(codec, Color.Blue)
      roundtrip(codec, Color.Reserved(255))
      roundtrip(codec, Color.Reserved(4))
    }

    "support building a codec for an enumeration with preserved reserved values, and reserved values are not in the type hierarchy" in {
      enum Color {
        case Red, Green, Blue
        case Reserved(value: Int)
      }

      val nonReserved: Codec[Color] = mappedEnum(uint8, Color.Red -> 1, Color.Green -> 2, Color.Blue -> 3)
      val reserved: Codec[Color.Reserved] = uint8.as[Color.Reserved]
      val codec: Codec[Either[Color.Reserved, Color]] = choice(
        nonReserved.xmapc(Right.apply)(_.toOption.get).upcast[Either[Color.Reserved, Color]],
        reserved.xmapc(Left.apply)(_.swap.toOption.get).upcast[Either[Color.Reserved, Color]]
      )

      roundtrip(codec, Right(Color.Red))
      roundtrip(codec, Right(Color.Green))
      roundtrip(codec, Right(Color.Blue))
      roundtrip(codec, Left(new Color.Reserved(255)))
      roundtrip(codec, Left(new Color.Reserved(4)))
    }

    "support building a codec for an ADT" in {
      enum Direction {
        case Stay
        case Go(units: Int)
      }

      val codec =
        discriminated[Direction].by(uint8).singleton(0, Direction.Stay).typecase(1, int32.as[Direction.Go])

      roundtrip(codec, Direction.Stay)
      roundtrip(codec, Direction.Go(42))
    }

    "support building a codec for recusive ADTs - e.g., trees" in {
      enum Tree {
        case Node(l: Tree, r: Tree)
        case Leaf(n: Int)
      }
      import Tree.{Node, Leaf}

      def treeCodec: Codec[Tree] = lazily {
        discriminated[Tree]
          .by(bool)
          .typecase(false, int32.as[Leaf])
          .typecase(true, (treeCodec :: treeCodec).as[Node])
      }

      roundtrip(treeCodec, Leaf(42))
      roundtrip(treeCodec, Node(Leaf(42), Node(Leaf(1), Leaf(2))))
    }

    "error when matching discriminator for encoding is not found" in {
      val codec =
        discriminated[AnyVal]
          .by(uint8)
          .typecase(0, bool)

      roundtrip(codec, true)
      roundtrip(codec, false)
      encodeError(codec, 1, new Err.MatchingDiscriminatorNotFound(1))
      encodeError(codec, Int.MaxValue, new Err.MatchingDiscriminatorNotFound(Int.MaxValue))
    }

    "support framing value codecs" in {
      sealed trait Direction
      case object Stay extends Direction
      case class Go(units: Int) extends Direction
      case class Annotate(message: String) extends Direction

      val stayCodec = provide(Stay)
      val goCodec = int32.as[Go]
      val annotateCodec = ascii.as[Annotate]

      val codec =
        discriminated[Direction]
          .by(uint8)
          .typecase(0, stayCodec)
          .typecase(1, goCodec)
          .typecase(2, annotateCodec)
          .framing([x] => (c: Codec[x]) => variableSizeBytes(uint8, c))

      roundtrip(list(codec), List(Stay, Go(1), Annotate("Hello"), Go(2), Stay))
    }
  }
}
