package scodec
package examples

import scodec.bits._
import codecs._

class AdtExample extends CodecSuite {

  sealed trait Sprocket derives Codec
  case class Woozle(count: Int, strength: Int) extends Sprocket
  case class Wocket(size: Int, inverted: Boolean) extends Sprocket

  "ADT codec examples" should {
    
    "support derivation" in {
      val codec = Codec[Sprocket]
      val encodedWocket = codec.encode(Wocket(1, true)).require
      assertBitsEqual(encodedWocket, 0x0100000001ff)
    }
  }
}
