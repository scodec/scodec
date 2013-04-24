scodec
======

Scala combinator library for working with binary data.

Design Constraints
------------------

This library focuses on contract-first and pure functional encoding and decoding of binary data.
The following design constraints are considered:
 - Binary structure should mirror protocol definitions and be self-evident under casual reading
 - Mapping binary structures to types should be statically verified
 - Encoding and decoding should be purely functional
 - Failures in encoding and decoding should provide descriptive errors
 - Compiler plugin should not be used

As a result, the library is implemented as a combinator based DSL.
Performance is considered but yields to the above design constraints.

Acknowledgements
----------------
The library uses [Scalaz](https://github.com/scalaz/scalaz) and [Shapeless](https://github.com/milessabin/shapeless)
and is heavily influenced by scala.util.parsing.combinator.


Introduction
------------

The primary abstraction is a [`Codec[A]`](src/main/scala/scodec/Codec.scala), which supports encoding a value of type `A` to a
[`BitVector`](src/main/scala/scodec/BitVector.scala) and decoding a `BitVector` to a value of type `A`.

The [`Codecs`](src/main/scala/scodec/Codecs.scala) object provides a number of predefined codecs and combinators.

```scala
    import scodec._
    import Codecs._
    import scalaz.\/

    // Create a codec for an 8-bit unsigned int followed by an 8-bit unsigned int followed by a 16-bit unsigned int
    val firstCodec = (uint8 ~ uint8 ~ uint16)

    // Decode a bit vector using that codec
    val result: String \/ (Int ~ Int ~ Int) = Codec.decode(firstCodec, BitVector(0x10, 0x2a, 0x03, 0xff))

    // Sum the result
    val add3 = (_: Int) + (_: Int) + (_: Int)
    val sum: String \/ Int = result map add3
```

Automatic case class binding is supported via Shapeless HLists:

```scala
    import shapeless._

    case class Point(x: Int, y: Int, z: Int)
    implicit val pointIso = Iso.hlist(Point.apply _, Point.unapply _)

    val pointCodec = (int8 :~: int8 :~: int8).as[Point]

    val encoded: String \/ BitVector = pointCodec.encode(Point(-5, 10, 1))
    // \/-(BitVector(24 bits, 0xfb0a01))

    val decoded: String \/ Point = Codec.decode(pointCodec, BitVector(0xfb, 0x0a, 0x01))
    // \/-(Point(-5,10,1))
```

Codecs can also be implicitly resolved, resulting in usage like:

```scala
    // Assuming Codec[Point] is in implicit scope

    val encoded = Codec.encode(Point(-5, 10, 1))
    // \/-(BitVector(24 bits, 0xfb0a01))

    val decoded = Codec.decode[Point](BitVector(0xfb, 0x0a, 0x01))
    // \/-(Point(-5,10,1))
```


Examples
--------

There are various examples in the test directory, including codecs for:

 - [MPEG Packets](src/test/scala/scodec/MpegPacketExample.scala)


Getting JAR
-----------

The library will be published on Maven Central once it reaches a critical mass of functionality.


Building
--------

This project uses sbt. To build, run `sbt publish-local`.
