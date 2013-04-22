package scodec

import scala.collection.GenTraversable

object Bytes {

  private val HexChars: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  def toHexadecimal(bytes: GenTraversable[Byte], includePrefix: Boolean = true): String = {
    val bldr = new StringBuilder
    if (includePrefix) bldr.append("0x")
    bytes foreach { b =>
      bldr.append(HexChars(b >> 4 & 0x0f)).append(HexChars(b & 0x0f))
    }
    bldr.toString
  }
}
