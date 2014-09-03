package scodec

import shapeless._
import HListOps._

import org.scalatest.{ FunSuite, Matchers }

class HListOpsTest extends FunSuite with Matchers {

  test("reUnit") {
    val hnil = (HNil: HNil)
    hnil.reUnit[HNil] shouldBe HNil
    hnil.reUnit[Unit :: HNil] shouldBe () :: HNil
    (1 :: HNil).reUnit[Int :: HNil] shouldBe (1 :: HNil)
    (1 :: HNil).reUnit[Int :: Unit :: HNil] shouldBe (1 :: () :: HNil)
    (1 :: 2 :: HNil).reUnit[Unit :: Int :: Unit :: Int :: HNil] shouldBe (() :: 1 :: () :: 2 :: HNil)

    "reUnit[Unit :: HNil, Unit :: HNil]" shouldNot compile
  }
}
