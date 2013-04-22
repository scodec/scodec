package scodec

import shapeless._

import scalaz.std.option.{optionInstance, some, none}

import org.scalatest._

class HListTest extends FunSuite with Matchers {

  import HListSyntax._

  test("sequence options") {
    val x = some(1) :: some("test") :: some(true) :: HNil
    val y = x.sequence shouldBe some(1 :: "test" :: true :: HNil)
  }

}
