package scodec.codecs

import org.scalacheck._
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scodec.bits.ByteVector

/**
 * Test the Fletcher checksum functionality
 *
 */
class FletcherChecksumTest extends WordSpec with GeneratorDrivenPropertyChecks {

  "fletcher16 checksum" should {
    /**
     *  http://en.wikipedia.org/wiki/Fletcher's_checksum
     *
     *  "Example calculation of the Fletcher-16 checksum"
     *
     */
    "wikipedia" in {
      val signer = ChecksumFactory.fletcher16.newSigner
      signer.update(Array(0x01, 0x02))
      assert(signer.verify(Array(0x04, 0x03)))
    }

    "0xAA * (3N+2) => Array(0x0,0x55)" in {
      forAll(Gen.posNum[Int]) { (n: Int) =>
        pattern(n, 2,
          Array(0x0, 0x55))
      }
    }

    "0xAA * (3N + 1) => Array(0xAA, 0xAA)" in {
      forAll(Gen.posNum[Int]) { (n: Int) =>
        pattern(n, 1,
          Array(0xAA.asInstanceOf[Byte], 0xAA.asInstanceOf[Byte]))
      }
    }

    "0xAA * (3N) => Array(0x0, 0x0)" in {
      forAll(Gen.posNum[Int]) { (n: Int) =>
        pattern(n, 0,
          Array(0x0, 0x0))
      }
    }
  }

  private def pattern(n: Int, delta: Int, expected: Array[Byte]): Unit = {
    val signer = ChecksumFactory.fletcher16.newSigner
    signer.update(ByteVector.fill(3 * n + delta)(0xAA).toArray)
    assert(signer.verify(expected))
  }

}