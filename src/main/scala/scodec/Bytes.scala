package scodec

import scala.collection.GenTraversable

import scalaz.\/
import scalaz.std.AllInstances._
import scalaz.syntax.id._
import scalaz.syntax.traverse._


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

  def fromHexadecimal(str: String): String \/ ByteVector = {
    val withoutPrefix = if (str startsWith "0x") str.substring(2) else str
    withoutPrefix.replaceAll("\\s", "").sliding(2, 2).toVector.map { h =>
      try java.lang.Integer.valueOf(h, 16).toByte.right
      catch { case e: NumberFormatException => s"Invalid octet '$h'".left }
    }.sequenceU.map { v => ByteVector(v) }
  }

  def fromValidHexadecimal(str: String): ByteVector =
    fromHexadecimal(str) valueOr { msg => throw new IllegalArgumentException(msg) }
}
