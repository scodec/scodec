package scodec
package examples

import scodec.bits._
import codecs._

class TlvExample extends CodecSuite {
  // Example of implementing type-length-value encodings using coproducts.
  // See http://en.wikipedia.org/wiki/Type-length-value for more information on TLV.
  //
  // This example models a command set for 2 dimensional movement. Each command
  // is represented in TLV form.
  //
  // The type field is a 1 byte uint8 with the following mappings:
  //  - 0 = GO command, followed by a length of 0 and no value
  //  - 1 = STOP command, followed by a length of 0 and no value
  //  - 2 = TURN LEFT command, followed by a length of 1-2 and whose value is a uint8 or uint16,
  //        representing an integer number of degrees to turn
  //  - 3 = TURN RIGHT command, followed by a length of 1-2 and whose value is a uint8 or uint16,
  //        representing an integer number of degrees to turn
  //
  // The length field is a 1 byte uint8.
  //
  // Unrecognized commands must be decoded.

  sealed trait Command
  case object Go extends Command
  case object Stop extends Command
  case class TurnLeft(degrees: Int) extends Command
  case class TurnRight(degrees: Int) extends Command

  case class UnrecognizedCommand(commandType: Int, data: BitVector)

  "type-length-value encodings" should {

    "be supported by coproduct codecs" in {
      // The type field acts as the discriminator for the coproduct
      // The length field is handled by coproduct framing
      // Each component codec writes the value

      val uint8or16: Codec[Int] = new Codec[Int] {
        def sizeBound = SizeBound.bounded(16, 24)
        def encode(i: Int) =
          uint8.encode(i).orElse(uint16.encode(i))
        def decode(b: BitVector) =
          uint16.decode(b).orElse(uint8.decode(b))
      }

      implicit val goCodec: Codec[Go.type] = provide(Go)
      implicit val stopCodec: Codec[Stop.type] = provide(Stop)
      implicit val leftCodec: Codec[TurnLeft] = uint8or16.as[TurnLeft]
      implicit val rightCodec: Codec[TurnRight] = uint8or16.as[TurnRight]
      implicit val unrecognizedCodec: Codec[UnrecognizedCommand] =
        (uint8 :: variableSizeBytes(uint8, bits)).as[UnrecognizedCommand]

      implicit val commandDiscriminated: Discriminated[Command, Int] =
        Discriminated[Command, Int](uint8, new CodecTransformation {
          def apply[X](c: Codec[X]) = variableSizeBytes(uint8, c)
        })
      implicit val goDiscriminator: Discriminator[Command, Go.type, Int] = Discriminator(0)
      implicit val stopDiscriminator: Discriminator[Command, Stop.type, Int] = Discriminator(1)
      implicit val leftDiscriminator: Discriminator[Command, TurnLeft, Int] = Discriminator(2)
      implicit val rightDiscriminator: Discriminator[Command, TurnRight, Int] = Discriminator(3)

      val codec: Codec[Either[UnrecognizedCommand, Command]] =
        discriminatorFallback(unrecognizedCodec, implicitly[Codec[Command]])

      roundtrip(codec, Right(Go))
      roundtrip(codec, Right(Stop))
      roundtrip(codec, Right(TurnLeft(270)))
      roundtrip(codec, Right(TurnRight(180)))
      codec.decode(hex"0400".bits).require.value shouldBe Left(
        UnrecognizedCommand(4, BitVector.empty)
      )
      roundtrip(list(codec), List(Right(TurnRight(180)), Right(Go), Right(Stop)))
    }
  }
}
