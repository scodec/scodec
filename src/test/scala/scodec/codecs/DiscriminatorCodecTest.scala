package scodec
package codecs

import scalaz.syntax.id._

class DiscriminatorCodecTest extends CodecSuite {

  test("type discriminator using syntax") {
    val codec = discriminated[AnyVal].by(uint8).using(typeDiscriminator(
      typeDiscriminatorCase(0, int32),
      typeDiscriminatorCase(1, bool)
    ))

    roundtrip(codec, true)
    roundtrip(codec, false)
    roundtrip(codec, 1)
    roundtrip(codec, Int.MaxValue)
  }

  test("type discriminator using partial functions") {
    val codec = discriminated[AnyVal].by(uint8).using({
      case _: Int => 0
      case _: Boolean => 1
    }, {
      case 0 => int32
      case 1 => bool
    })

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
    val goCodec = int32.xmap[Go](Go.apply, _.units)

    val codec = discriminated[Direction] by uint8 using typeDiscriminator(
      typeDiscriminatorCase(0, stayCodec),
      typeDiscriminatorCase(1, goCodec)
    )

    roundtrip(codec, Stay)
    roundtrip(codec, Go(42))
  }
}
