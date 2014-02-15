package scodec
package codecs

import scalaz.syntax.id._

class DiscriminatorCodecTest extends CodecSuite {

  test("type discriminator using partial functions and subtyping") {
    val codec =
      discriminated[AnyVal].by(uint8)
      .\ (0) { case i: Int => i } (int32)
      .\ (1) { case b: Boolean => b } (bool)
      .build

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
      .build

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
      .build

    roundtrip(codec, Stay)
    roundtrip(codec, Go(42))
  }
}
