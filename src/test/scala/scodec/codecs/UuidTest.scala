package scodec
package codecs

import java.util.UUID
import org.scalacheck.Arbitrary

class UuidTest extends CodecSuite {

  implicit val arbitraryUuid: Arbitrary[UUID] = Arbitrary(UUID.randomUUID)

  test("roundtrip") {
    forAll { (u: UUID) => roundtrip(uuid, u) }
  }
}
