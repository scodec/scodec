package scodec

import org.scalatest.{ FunSuite, Matchers }

import Codecs._

class DiscriminatorTest extends FunSuite with Matchers {
  test("simple") {
    val d = typeDiscriminator[AnyVal, Int](
      typeDiscriminatorCase(0, int32),
      typeDiscriminatorCase(1, bool)
    )
    d.discriminate(42) shouldBe Some(0)
    d.discriminate(true) shouldBe Some(1)
    d.discriminate(42L) shouldBe None
  }
}
