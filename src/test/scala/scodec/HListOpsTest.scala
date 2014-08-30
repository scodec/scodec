package scodec

import shapeless._
import HListOps._

import org.scalatest.{ FunSuite, Matchers }

class HListOpsTest extends FunSuite with Matchers {

  test("reUnit") {
    reUnit[HNil, HNil](HNil) shouldBe (HNil)
    reUnit[Unit :: HNil, HNil](HNil) shouldBe (() :: HNil)
    reUnit[Unit :: HNil, HNil](HNil) shouldBe (() :: HNil)
    (1 :: HNil).reUnit[Int :: HNil] shouldBe (1 :: HNil)
    (1 :: HNil).reUnit[Int :: Unit :: HNil] shouldBe (1 :: () :: HNil)
    (1 :: 2 :: HNil).reUnit[Unit :: Int :: Unit :: Int :: HNil] shouldBe (() :: 1 :: () :: 2 :: HNil)

    "reUnit[Unit :: HNil, Unit :: HNil]" shouldNot compile // because L is the result of filtering Units out of K
  }
}
