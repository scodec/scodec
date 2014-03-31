package scodec
package codecs

import scalaz.syntax.either._

class DiscriminatorCodecTest extends CodecSuite {

  test("type discriminator using typecases") {
    val codec =
      discriminated[AnyVal].by(uint8)
      .typecase(0, int32)
      .typecase(1, bool)

    roundtrip(codec, true)
    roundtrip(codec, false)
    roundtrip(codec, 1)
    roundtrip(codec, Int.MaxValue)
  }

  test("type discriminator using partial functions and subtyping") {
    val codec =
      discriminated[AnyVal].by(uint8)
      .\ (0) { case i: Int => i } (int32)
      .\ (1) { case b: Boolean => b } (bool)

    roundtrip(codec, true)
    roundtrip(codec, false)
    roundtrip(codec, 1)
    roundtrip(codec, Int.MaxValue)
  }

  test("type discriminator using A => Option[B] and subtyping") {
    val codec =
      discriminated[AnyVal].by(uint8)
      ./ (0) { v => v match { case i: Int => Some(i); case _ => None }} (int32)
      ./ (1) { v => v match { case b: Boolean => Some(b); case _ => None }} (bool)

    roundtrip(codec, true)
    roundtrip(codec, false)
    roundtrip(codec, 1)
    roundtrip(codec, Int.MaxValue)
  }

  test("enumeration example") {

    sealed trait Direction
    case object Stay extends Direction
    case class Go(units: Int) extends Direction

    val stayCodec = provide(Stay)
    val goCodec = int32.pxmap[Go](Go.apply, Go.unapply)

    val codec =
      discriminated[Direction].by(uint8)
      .\ (0) { case s@Stay => s } (stayCodec)
      .\ (1) { case g@Go(_) => g } (goCodec)

    roundtrip(codec, Stay)
    roundtrip(codec, Go(42))
  }

  test("tree example") {
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
