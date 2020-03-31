package scodec
package codecs

import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

class RecoverCodecTest extends CodecSuite {

  property("recover - always code a value for true value") {
    forAll { (i: Int) =>
      val codec = recover(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b2, rest)) = for {
        bits <- codec.encode(true)
        result <- codec.decode(bits)
      } yield result
      assert(rest.isEmpty)
      assert(b2)
    }
  }

  property("recover - always code an empty vector for false value") {
    forAll { (i: Int) =>
      val codec = recover(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b2, rest)) = for {
        bits <- codec.encode(false)
        result <- codec.decode(bits)
      } yield result
      assert(rest.isEmpty)
      assert(!b2)
    }
  }

  property("recover - decode a false when target codec fails to decode") {
    forAll { (i: Int) =>
      val codec = recover(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.empty)
      assert(rest.isEmpty)
      assert(!b)
    }
  }

  property("recover - decode the wrong value as false and backtrack") {
    forAll { (i1: Int, i2: Int) =>
      if (i1 != i2 && i1 >= 0 && i2 >= 0) {
        val codec = recover(constant(BitVector.fromInt(i1)))
        val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.fromInt(i2))
        assertEquals(rest, BitVector.fromInt(i2))
        assert(!b)
      }
    }
  }

  property("lookahead - always code a value for true value") {
    forAll { (i: Int) =>
      val codec = lookahead(constant(BitVector.fromInt(i)))
      val Attempt.Successful((encoded, DecodeResult(b2, rest))) = for {
        bits <- codec.encode(true)
        result <- codec.decode(bits)
      } yield (bits, result)
      assertEquals(rest, encoded)
      assert(b2)
    }
  }

  property("lookahead - always code an empty vector for false value") {
    forAll { (i: Int) =>
      val codec = lookahead(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b2, rest)) = for {
        bits <- codec.encode(false)
        result <- codec.decode(bits)
      } yield result
      assert(rest.isEmpty)
      assert(!b2)
    }
  }

  property("lookahead - decode a false when target codec fails to decode") {
    forAll { (i: Int) =>
      val codec = lookahead(constant(BitVector.fromInt(i)))
      val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.empty)
      assert(rest.isEmpty)
      assert(!b)
    }
  }

  property("lookahead - decode the wrong value as false and backtrack") {
    forAll { (i1: Int, i2: Int) =>
      if (i1 != i2 && i1 >= 0 && i2 >= 0) {
        val codec = lookahead(constant(BitVector.fromInt(i1)))
        val Attempt.Successful(DecodeResult(b, rest)) = codec.decode(BitVector.fromInt(i2))
        assertEquals(rest, BitVector.fromInt(i2))
        assert(!b)
      }
    }
  }
}
