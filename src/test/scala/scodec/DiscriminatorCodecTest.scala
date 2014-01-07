package scodec

import scalaz.syntax.id._

import Codecs._


class DiscriminatorCodecTest extends CodecSuite {

  test("AnyVal example") {
    val codec = discriminated[AnyVal].by(uint8) {
      case _: Int => 0
      case _: Boolean => 1
    }.withCodecs {
      case 0 => int32
      case 1 => bool
    }

    roundtrip(codec, true)
    roundtrip(codec, false)
    roundtrip(codec, 1)
    roundtrip(codec, Int.MaxValue)
  }

  test("enumeration example") {

    sealed trait Direction
    case object Stay extends Direction
    case class Go(units: Int) extends Direction

    val codec = discriminated[Direction].by(uint8) {
      case Stay => 0
      case Go(_) => 1
    }.withCodecs {
      case 0 => provide(Stay)
      case 1 => int32.xmap[Go](Go.apply, _.units)
    }

    roundtrip(codec, Stay)
    roundtrip(codec, Go(42))
  }
}
