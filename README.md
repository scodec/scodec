scodec
======

[![Discord](https://img.shields.io/discord/632277896739946517.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/wKn3cpfRVz)
[![Maven Central](https://img.shields.io/maven-central/v/org.scodec/scodec-core_3)](https://maven-badges.herokuapp.com/maven-central/org.scodec/scodec-core_3)
[![javadoc](https://javadoc.io/badge2/org.scodec/scodec-core_3/javadoc.svg)](https://javadoc.io/doc/org.scodec/scodec-core_3)

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
Scodec 1.x used [Shapeless](https://github.com/milessabin/shapeless)
and is heavily influenced by scala.util.parsing.combinator. As of Scodec 2.x, the library only
depends on the standard library.

Administrative
--------------

This project is licensed under a [3-clause BSD license](LICENSE).

The [scodec channel on Typelevel Discord](https://discord.gg/wKn3cpfRVz) is a good place to go for help.

Introduction
------------

The primary abstraction is a [`Codec[A]`](shared/src/main/scala/scodec/Codec.scala), which supports encoding a value of type `A` to a
`BitVector` and decoding a `BitVector` to a value of type `A`.

The [`codecs`](shared/src/main/scala/scodec/codecs.scala) objects provides a number of predefined codecs and combinators.

```scala
    import scodec.*
    import scodec.bits.*
    import scodec.codecs.*

    // Create a codec for an 8-bit unsigned int followed by an 8-bit unsigned int followed by a 16-bit unsigned int
    val firstCodec = uint8 :: uint8 :: uint16

    // Decode a bit vector using that codec
    val result: Attempt[DecodeResult[(Int, Int, Int)]] = firstCodec.decode(hex"102a03ff".bits)
    // Successful(DecodeResult(((16, 42), 1023), BitVector(empty)))

    // Sum the result
    val add3 = (_: Int) + (_: Int) + (_: Int)
    val sum: Attempt[DecodeResult[Int]] = result.map(_.map(add3))
    // Successful(DecodeResult(1081, BitVector(empty)))
```

Automatic case class binding is supported via tuples:

```scala
    case class Point(x: Int, y: Int, z: Int)

    val pointCodec: Codec[Point] = (int8 :: int8 :: int8).as[Point]

    val encoded: Attempt[BitVector] = pointCodec.encode(Point(-5, 10, 1))
    // Successful(BitVector(24 bits, 0xfb0a01))

    val decoded: Attempt[DecodeResult[Point]] = pointCodec.decode(0xfb0a01)
    // Successful(DecodeResult(Point(-5, 10, 1), BitVector(empty)))
```

Codecs can also be derived, resulting in usage like:

```scala
    case class Point(x: Int, y: Int, z: Int) derives Codec

    val encoded: Attempt[BitVector] = Codec.encode(Point(-5, 10, 1))
    // Successful(BitVector(96 bits, 0x000000fb0000000a00000001))

    val decoded: Attempt[DecodeResult[Point]] = Codec.decode[Point](0x000000fb0000000a00000001)
    // Successful(DecodeResult(Point(-5, 10, 1), BitVector(empty)))
```

New codecs can be created by either implementing the `Codec` trait though typically new codecs are created by applying one or more combinators to existing codecs.

See [the guide](http://scodec.org/guide/) for detailed documentation. Also, see [ScalaDoc](https://javadoc.io/doc/org.scodec/scodec-core_3). Especially:
 - [`Codec`](https://javadoc.io/doc/org.scodec/scodec-core_3/latest/scodec/Codec.html)
 - [`codecs` package](https://javadoc.io/doc/org.scodec/scodec-core_3/latest/scodec/codecs.html)

Ecosystem
---------

Many libraries have support for scodec:
  - [FS2](https://github.com/typelevel/fs2)
  - [Circe](https://github.com/circe/circe)
  - [Refined](https://github.com/fthomas/refined)

Examples
--------

There are various examples in the test directory, including codecs for:

 - [UDP Datagrams](unitTests/shared/src/test/scala/scodec/examples/UdpDatagramExample.scala)
 - [MPEG Packets](unitTests/shared/src/test/scala/scodec/examples/MpegPacketExample.scala)
 - [libpcap Files](unitTests/jvm/src/test/scala/scodec/examples/PcapExample.scala)

The [protocols](https://github.com/typelevel/fs2/tree/main/protocols) module of fs2 has production
quality codecs for the above examples.

The [skunk](https://github.com/tpolecat/skunk) database library uses scodec to communicate with Postgres.

The [hippo](https://github.com/indoorvivants/hippo) project uses scodec to parse .hprof files.

The [bitcoin-scodec](https://github.com/yzernik/bitcoin-scodec) library has a codec for the Bitcoin network protocol.

The [scodec-msgpack](https://github.com/pocketberserker/scodec-msgpack) library provides
codecs for [MessagePack](http://msgpack.org/).

The [fs2-http](https://github.com/Spinoco/fs2-http) project uses FS2, scodec, and shapeless to implement a minimal HTTP client and server.

The [scodec-bson](https://gitlab.com/lJoublanc/scodec-bson) library implements [BSON](http://bsonspec.org) codecs and combinators.

Testing Your Own Codecs
-----------------------

If you're creating your own `Codec` instances scodec publishes some of its own test tooling in the `scodec-testkit` module.

Getting Binaries
----------------

```scala
libraryDependencies += "org.scodec" %%% "scodec-core" % 
  (if (scalaVersion.value.startsWith("2.")) "1.11.9" else "2.1.0")
```

Building
--------

This project uses sbt and requires node.js to be installed in order to run Scala.js tests. To build, run `sbt publishLocal`.

Code of Conduct
---------------

See the [Code of Conduct](CODE_OF_CONDUCT.md).

