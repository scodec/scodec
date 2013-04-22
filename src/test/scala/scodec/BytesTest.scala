package scodec

import org.scalatest._

class BytesTest extends FunSuite with Matchers {
  test("toHexadecimal") {
    val arr = Array[Byte](0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte)
    Bytes.toHexadecimal(arr) should be ("0xdeadbeef")
  }
}
