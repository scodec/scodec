package scodec
package codecs

import java.util.UUID
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class UuidTest extends CodecSuite {

  implicit val arbitraryUuid: Arbitrary[UUID] = Arbitrary(UUID.randomUUID.nn)

  property("roundtrip") {
    forAll((u: UUID) => roundtrip(uuid, u))
  }
}
