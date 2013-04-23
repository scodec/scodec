package scodec

import scalaz.syntax.id._

import org.scalatest._


class BytesTest extends FunSuite with Matchers {
  val deadbeef = ByteVector(0xde, 0xad, 0xbe, 0xef)

  test("toHexadecimal") {
    deadbeef.toHexadecimal should be ("0xdeadbeef")
    Bytes.toHexadecimal(deadbeef.toIterable) should be ("0xdeadbeef")
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
