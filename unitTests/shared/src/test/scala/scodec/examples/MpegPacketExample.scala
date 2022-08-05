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

import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.*

// Define MPEG codecs
object MpegCodecs:

  // Define case classes that describe MPEG packets
  //
  // Note: This encoding is a simplified version of what's required to parse real
  // MPEG packets. For a production quality codec, see the
  // [Packet](https://github.com/scodec/scodec-protocols/blob/series/1.0.x/src/main/scala/scodec/protocols/mpeg/transport/Packet.scala)
  // and associated data types from scodec-protocols.

  case class TransportStreamHeader(
      transportStringIndicator: Boolean,
      payloadUnitStartIndicator: Boolean,
      transportPriority: Boolean,
      pid: Int,
      scramblingControl: Int,
      adaptationFieldControl: Int,
      continuityCounter: Int
  ):
    def adaptationFieldIncluded: Boolean = adaptationFieldControl >= 2
    def payloadIncluded: Boolean = adaptationFieldControl == 1 || adaptationFieldControl == 3

  case class AdaptationFieldFlags(
      discontinuity: Boolean,
      randomAccess: Boolean,
      priority: Boolean,
      pcrFlag: Boolean,
      opcrFlag: Boolean,
      splicingPointFlag: Boolean,
      transportPrivateDataFlag: Boolean,
      adaptationFieldExtension: Boolean
  )

  case class AdaptationField(
      flags: AdaptationFieldFlags,
      pcr: Option[BitVector],
      opcr: Option[BitVector],
      spliceCountdown: Option[Int]
  )

  case class MpegPacket(
      header: TransportStreamHeader,
      adaptationField: Option[AdaptationField],
      payload: Option[ByteVector]
  )

  val tsHeaderCodec: Codec[TransportStreamHeader] = fixedSizeBytes(
    4,
    ("syncByte" | constant(0x47)) ~> (("transportStringIndicator" | bool) ::
      ("payloadUnitStartIndicator" | bool) ::
      ("transportPriority" | bool) ::
      ("pid" | uint(13)) ::
      ("scramblingControl" | uint2) ::
      ("adaptationFieldControl" | uint2) ::
      ("continuityCounter" | uint4))
  ).as[TransportStreamHeader]

  val adaptationFieldFlagsCodec: Codec[AdaptationFieldFlags] = fixedSizeBytes(
    1,
    ("discontinuity" | bool) ::
      ("randomAccess" | bool) ::
      ("priority" | bool) ::
      ("pcrFlag" | bool) ::
      ("opcrFlag" | bool) ::
      ("splicingPointFlag" | bool) ::
      ("transportPrivateDataFlag" | bool) ::
      ("adaptationFieldExtension" | bool)
  ).as[AdaptationFieldFlags]

  val adaptationFieldCodec: Codec[AdaptationField] =
    ("adaptation_flags" | adaptationFieldFlagsCodec)
      .flatPrepend { flags =>
        ("pcr" | conditional(flags.pcrFlag, bits(48))) ::
          ("opcr" | conditional(flags.opcrFlag, bits(48))) ::
          ("spliceCountdown" | conditional(flags.splicingPointFlag, int8))
      }
      .as[AdaptationField]

  val packetCodec: Codec[MpegPacket] =
    ("header" | tsHeaderCodec)
      .flatPrepend { hdr =>
        ("adaptation_field" | conditional(hdr.adaptationFieldIncluded, adaptationFieldCodec)) ::
          ("payload" | conditional(hdr.payloadIncluded, bytes(184)))
      }
      .as[MpegPacket]

class MpegPacketExample extends CodecSuite:

  import MpegCodecs.*

  test("MpegPacket codec") {
    val pkt = MpegPacket(
      TransportStreamHeader(false, true, false, 0, 0, 1, 15),
      None,
      Some(BitVector.low(184 * 8).toByteVector)
    )
    val encoded = packetCodec.encode(pkt).require
    val decoded = packetCodec.decode(encoded).require.value
    assertEquals(decoded, pkt)
  }
