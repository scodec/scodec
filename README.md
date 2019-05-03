scodec
======

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scodec/scodec?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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
The library uses [Shapeless](https://github.com/milessabin/shapeless)
and is heavily influenced by scala.util.parsing.combinator.

Administrative
--------------

This project is licensed under a [3-clause BSD license](LICENSE).

The [scodec mailing list](https://groups.google.com/forum/#!forum/scodec) contains release announcements and is generally a good place to go for help. Also consider using the [scodec tag on StackOverflow](http://stackoverflow.com/questions/tagged/scodec).

Introduction
------------

The primary abstraction is a [`Codec[A]`](shared/src/main/scala/scodec/Codec.scala), which supports encoding a value of type `A` to a
`BitVector` and decoding a `BitVector` to a value of type `A`.

The [`codecs`](shared/src/main/scala/scodec/codecs/package.scala) package provides a number of predefined codecs and combinators.

```scala
    import scodec._
    import scodec.bits._
    import codecs._

    // Create a codec for an 8-bit unsigned int followed by an 8-bit unsigned int followed by a 16-bit unsigned int
    val firstCodec = uint8 ~ uint8 ~ uint16

    // Decode a bit vector using that codec
    val result: Attempt[DecodeResult[(Int ~ Int ~ Int)]] = firstCodec.decode(hex"102a03ff".bits)
    // Successful(DecodeResult(((16, 42), 1023), BitVector(empty)))

    // Sum the result
    val add3 = (_: Int) + (_: Int) + (_: Int)
    val sum: Attempt[DecodeResult[Int]] = result.map(_.map(add3))
    // Successful(DecodeResult(1081, BitVector(empty)))
```

Automatic case class binding is supported via Shapeless HLists:

```scala
    case class Point(x: Int, y: Int, z: Int)

    val pointCodec = (int8 :: int8 :: int8).as[Point]

    val encoded: Attempt[BitVector] = pointCodec.encode(Point(-5, 10, 1))
    // Successful(BitVector(24 bits, 0xfb0a01))

    val decoded: Attempt[DecodeResult[Point]] = pointCodec.decode(hex"0xfb0a01".bits)
    // Successful(DecodeResult(Point(-5, 10, 1), BitVector(empty)))
```

Codecs can also be implicitly resolved, resulting in usage like:

```scala
    // Assuming Codec[Point] is in implicit scope

    val encoded: Attempt[BitVector] = Codec.encode(Point(-5, 10, 1))
    // Successful(BitVector(24 bits, 0xfb0a01))

    val decoded: Attempt[DecodeResult[Point]] = Codec.decode[Point](hex"0xfb0a01".bits)
    // Successful(DecodeResult(Point(-5, 10, 1), BitVector(empty)))
```

Note: by default, scodec provides no implicit codecs. Many sensible defaults can be imported via `import scodec.codecs.implicits._`, including codecs for various primitive types and some immutable collections.

New codecs can be created by either implementing the `Codec` trait or by passing an encoder function and decoder function to the `Codec` apply method. Typically, new codecs are created by applying one or more combinators to existing codecs.

See [the guide](http://scodec.org/guide/) for detailed documentation. Also, see [ScalaDoc](http://scodec.org/api/). Especially:
 - [`Codec`](http://scodec.org/api/scodec-core/1.11.1/#scodec.Codec)
 - [`codecs` package](http://scodec.org/api/scodec-core/1.11.1/#scodec.codecs.package)

Ecosystem
---------

Many libraries have support for scodec:
  - [FS2](https://github.com/functional-streams-for-scala)
  - [Circe](https://github.com/circe/circe)
  - [Refined](https://github.com/fthomas/refined)

Examples
--------

There are various examples in the test directory, including codecs for:

 - [UDP Datagrams](unitTests/src/test/scala/scodec/examples/UdpDatagramExample.scala)
 - [MPEG Packets](unitTests/src/test/scala/scodec/examples/MpegPacketExample.scala)
 - [libpcap Files](unitTests/src/test/scala/scodec/examples/PcapExample.scala)

The [scodec-protocols](https://github.com/scodec/scodec-protocols) has production
quality codecs for the above examples.

The [bmsg](https://github.com/lktkorg/bmsg) library has a codec for the Bitcoin Cash and Bitcoin Core network protocol.

The [scodec-msgpack](https://github.com/pocketberserker/scodec-msgpack) library provides
codecs for [MessagePack](http://msgpack.org/).

The [fs2-http](https://github.com/Spinoco/fs2-http) project uses FS2, scodec, and shapeless to implement a minimal HTTP client and server.

The [scodec-bson](https://gitlab.com/lJoublanc/scodec-bson) library implements [BSON](http://bsonspec.org) codecs and combinators.

Testing Your Own Codecs
-----------------------

If you're creating your own `Codec` instances scodec publishes some of its own test tooling in the `scodec-testkit` module.

Getting Binaries
----------------

See the [releases page on the website](http://scodec.org/releases/).

Building
--------

This project uses sbt and requires node.js to be installed in order to run Scala.js tests. To build, run `sbt publish-local`.

Code of Conduct
---------------

See the [Code of Conduct](CODE_OF_CONDUCT.md).

