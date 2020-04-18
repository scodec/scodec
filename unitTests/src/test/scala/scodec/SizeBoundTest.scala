package scodec

class SizeBoundTest extends CodecSuite {

  test("support addition of bounds") {
    assertEquals(SizeBound.exact(1) + SizeBound.exact(2), SizeBound.exact(3))
    assertEquals(SizeBound.exact(1) + SizeBound.unknown, SizeBound.atLeast(1))
    assertEquals(SizeBound.exact(1) + SizeBound.atLeast(2), SizeBound.atLeast(3))
    assertEquals(SizeBound.exact(1) + SizeBound.atMost(2), SizeBound.bounded(1, 3))
  }

  test("support multiplying a bound by a scalar") {
    assertEquals(SizeBound.exact(1) * 3, SizeBound.exact(3))
    assertEquals(SizeBound.unknown * 3, SizeBound.unknown)
    assertEquals(SizeBound.atLeast(2) * 6, SizeBound.atLeast(12))
    assertEquals(SizeBound.bounded(1, 5) * 10, SizeBound.bounded(10, 50))
  }

  test("support ORing of bounds") {
    assertEquals(SizeBound.exact(1) | SizeBound.exact(2), SizeBound.bounded(1, 2))
    assertEquals(SizeBound.exact(1) | SizeBound.unknown, SizeBound.unknown)
    assertEquals(SizeBound.unknown | SizeBound.exact(1), SizeBound.unknown)
    assertEquals(SizeBound.exact(1) | SizeBound.atLeast(2), SizeBound.atLeast(1))
    assertEquals(SizeBound.exact(1) | SizeBound.atMost(2), SizeBound.bounded(0, 2))
  }

  test("support choice (i.e., ORing a collection of bounds together)") {
    assertEquals(SizeBound.choice(List(SizeBound.exact(1), SizeBound.exact(2), SizeBound.exact(3))), SizeBound.bounded(1, 3))
    assertEquals(SizeBound.choice(Nil), SizeBound.exact(0))
  }

  test("prevent creation of non-sensical bounds") {
    intercept[IllegalArgumentException] { SizeBound(1, Some(0)) }
  }
}
