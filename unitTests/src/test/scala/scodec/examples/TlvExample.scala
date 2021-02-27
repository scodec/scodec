/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec
package examples

import scodec.bits.*
import codecs.*

class TlvExample extends CodecSuite:
  // Example of implementing type-length-value encodings.
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

  enum Command:
    case Go
    case Stop
    case TurnLeft(degress: Int)
    case TurnRight(degress: Int)

  case class UnrecognizedCommand(commandType: Int, data: BitVector)

  test("type-length-value encodings") {
    // The type field acts as the discriminator
    // The length field is handled by framing
    // Each component codec writes the value

    val uint8or16: Codec[Int] = new Codec[Int] {
      def sizeBound = SizeBound.bounded(16, 24)
      def encode(i: Int) =
        uint8.encode(i).orElse(uint16.encode(i))
      def decode(b: BitVector) =
        uint16.decode(b).orElse(uint8.decode(b))
    }

    val commandCodec: Codec[Command] =
      discriminated[Command].by(uint8)
        .framing([x] => (c: Codec[x]) => variableSizeBytes(uint8, c))
        .singleton(0, Command.Go)
        .singleton(1, Command.Stop)
        .typecase(2, uint8or16.as[Command.TurnLeft])
        .typecase(3, uint8or16.as[Command.TurnRight])

    val unrecognizedCodec: Codec[UnrecognizedCommand] =
      (uint8 :: variableSizeBytes(uint8, bits)).as[UnrecognizedCommand]

    val codec: Codec[Either[UnrecognizedCommand, Command]] =
      discriminatorFallback(unrecognizedCodec, commandCodec)

    roundtrip(codec, Right(Command.Go))
    roundtrip(codec, Right(Command.Stop))
    roundtrip(codec, Right(Command.TurnLeft(270)))
    roundtrip(codec, Right(Command.TurnRight(180)))
    assertEquals(codec.decode(hex"0400".bits).require.value, Left(
      UnrecognizedCommand(4, BitVector.empty)
    ))
    roundtrip(list(codec), List(Right(Command.TurnRight(180)), Right(Command.Go), Right(Command.Stop)))
  }
