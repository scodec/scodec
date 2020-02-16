package scodec
package examples

import scodec.bits._
import codecs._

class CoproductsExample extends CodecSuite {

  sealed trait Sprocket derives Codec
  case class Woozle(count: Int, strength: Int) extends Sprocket
  case class Wocket(size: Int, inverted: Boolean) extends Sprocket

  "coproduct codec examples" should {
    
    "support derivation" in {
      val codec = summon[Codec[Sprocket]]
      val encodedWocket = codec.encode(Wocket(1, true)).require
      assertBitsEqual(encodedWocket, 0x0100000001ff)
    }
  }
}
