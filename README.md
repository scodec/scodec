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

The library uses Scalaz and Shapeless.

Introduction
------------

The primary abstraction is a [`Codec[A]`](src/main/scala/scodec/Codec.scala), which supports encoding a value of type `A` to a
[`BitVector`](src/main/scala/scodec/BitVector.scala) and decoding a `BitVector` to a value of type `A`.

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
