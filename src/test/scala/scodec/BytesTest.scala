package scodec

import scalaz.syntax.id._

import org.scalatest._


class BytesTest extends FunSuite with Matchers {
  val deadbeef = Vector[Byte](0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte)

  test("toHexadecimal") {
    Bytes.toHexadecimal(deadbeef) should be ("0xdeadbeef")
    Bytes.toHexadecimal(deadbeef.toArray) should be ("0xdeadbeef")
  }

  test("fromHexadecimal") {
    Bytes.fromHexadecimal("0xdeadbeef") should be (deadbeef.right)
    Bytes.fromHexadecimal("0xDEADBEEF") should be (deadbeef.right)
    Bytes.fromHexadecimal("deadbeef") should be (deadbeef.right)
    Bytes.fromHexadecimal("DEADBEEF") should be (deadbeef.right)
    Bytes.fromHexadecimal("de ad be ef") should be (deadbeef.right)
    Bytes.fromHexadecimal("de\tad\nbe\tef") should be (deadbeef.right)
    Bytes.fromHexadecimal("garbage") should be ("Invalid octet 'ga'".left)
  }
}
