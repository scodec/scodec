package scodec

import java.util.UUID
import org.scalacheck.Arbitrary

import Codecs._


class UuidTest extends CodecSuite {

  implicit val arbitraryUuid: Arbitrary[UUID] = Arbitrary(UUID.randomUUID)

  test("roundtrip") {
    forAll { (u: UUID) => roundtrip(uuid, u) }
  }
}
