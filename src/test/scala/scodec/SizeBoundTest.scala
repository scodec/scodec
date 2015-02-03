package scodec

import org.scalatest.{ WordSpec, Matchers }

class SizeBoundTest extends WordSpec with Matchers {

  "the SizeBound class" should {

    "support addition of bounds" in {
      SizeBound.exact(1) + SizeBound.exact(2) shouldBe SizeBound.exact(3)
      SizeBound.exact(1) + SizeBound.unknown shouldBe SizeBound.atLeast(1)
      SizeBound.exact(1) + SizeBound.atLeast(2) shouldBe SizeBound.atLeast(3)
      SizeBound.exact(1) + SizeBound.atMost(2) shouldBe SizeBound.bounded(1, 3)
    }

    "support multiplying a bound by a scalar" in {
      SizeBound.exact(1) * 3 shouldBe SizeBound.exact(3)
      SizeBound.unknown * 3 shouldBe SizeBound.unknown
      SizeBound.atLeast(2) * 6 shouldBe SizeBound.atLeast(12)
      SizeBound.bounded(1, 5) * 10 shouldBe SizeBound.bounded(10, 50)
    }

    "support ORing of bounds" in {
      SizeBound.exact(1) | SizeBound.exact(2) shouldBe SizeBound.bounded(1, 2)
      SizeBound.exact(1) | SizeBound.unknown shouldBe SizeBound.unknown
      SizeBound.unknown | SizeBound.exact(1) shouldBe SizeBound.unknown
      SizeBound.exact(1) | SizeBound.atLeast(2) shouldBe SizeBound.atLeast(1)
      SizeBound.exact(1) | SizeBound.atMost(2) shouldBe SizeBound.bounded(0, 2)
    }

    "support choice (i.e., ORing a collection of bounds together)" in {
      SizeBound.choice(List(SizeBound.exact(1), SizeBound.exact(2), SizeBound.exact(3))) shouldBe SizeBound.bounded(1, 3)
      SizeBound.choice(Nil) shouldBe SizeBound.exact(0)
    }
  }
}
