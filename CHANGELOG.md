1.7.2
=====
 - Added `byteAligned` combinator, which right pads a `Codec[A]` such that the binary size is always evenly divisible by 8.
 - Added `decodeValue` back to `Decoder`, `Codec`, and `GenCodec`.
 - Added `limitedSizeBits` and `limitedSizeBytes` combinators, which behave like `fixedSizeBits`/`fixedSizeBytes` but perform no padding.
 - Added support for value codec framing to `DiscriminatorCodec` and auto-generated coproduct codecs. See `TlvExample` for usage.
 - Added `fallback` and `discriminatorFallback` combinators.
 - Added `variableSizePrefixed{Bits,Bytes}` and `variableSizePrefixed{Bits,Bytes}Long` combinators for working with binary shapes like `size ++ prefix ++ value` where `size` encodes the length of `value`.
 - The `.as[CaseClass]` syntax now automatically drops unit values from the `HList` representation.

1.7.1
=====
 - Added `string32`, `ascii32`, and `utf8_32` codecs, which encode a 32-bit 2s complement big endian size field before the string encoding.
 - Added various combinators for logging results of encoding/decoding -- `logBuilder`, `logFailuresBuilder`, `logSuccessesBuilder`, `logToStdOut`, and `logFailuresToStdOut`.
 - Added `zlib` combinator, which compresses the results of another codec.
 - Fixed bug where the contents of the scodec-bits jar was included in the scodec-core jar.

1.7.0
=====
 - Group id (ivy organization) changed from `org.typelevel` to `org.scodec`.
 - Upgraded to Shapeless 2.1 and removed the dependency on Scalaz. Users must use Shapeless 2.1.x,
   as the 2.1 series is not binary compatible with the 2.0 series.
 - As a result of the Shapeless 2.1 upgrade, Scala 2.10 users are required to use the Macro Paradise compiler
   plugin. Scala 2.11 users do not need Macro Paradise. For cross builds, the following SBT setting will add
   the plugin to only your 2.10 build:

```scala
   // Shapeless 2.1.0 on Scala 2.10 requires macro paradise
   libraryDependencies ++= {
     if (scalaBinaryVersion.value startsWith "2.10") Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)) else Nil
   }
```

 - Derived codecs now work for arbitrarily complex structures. In previous versions, deriving
   codecs for some complex types would result in implicit divergence, requiring, for instance,
   users to manually define implicit codecs for each component type before deriving a product
   codec. These limitations have been removed.
 - Related to the fixes for derivation of complex structures, the `ImplicitCodec` and `DerivedCodec`
   types have been removed in favor of `shapeless.Lazy[Codec[A]]`. If authoring combinators that
   require implicit codecs, use `Lazy[Codec[A]]` instead of `Codec[A]` or `ImplicitCodec[A]`.
   The same advice holds for implicit encoders and decoders -- use `Lazy[Encoder[A]]` and `Lazy[Decoder[A]]`.
 - All uses of `scalaz.\/` were replaced with `scodec.Attempt`, which is isomorphic to
   `Err \/ ?`. See the ScalaDoc for `Attempt` for combinators. Some specific notes:
   - The return type of `encode` has changed from `Err \/ BitVector` to `Attempt[BitVector]`.
   - The return type of `decode` has changed from `Err \/ (BitVector, A)` to `Attempt[DecodeResult[A]]`.
     `DecodeResult` is a case class that contains both the `A` value as well as the remainder `BitVector`.
     It contains two useful methods, `map` and `mapRemainder`, which makes it nicer to work with than `Tuple2`.
   - Replace `codec.encodeValid(a)` with `codec.encode(a).require`
   - Replace `codec.decodeValid(a)` with `codec.decode(a).require`
   - Replace `codec.decodeValidValue(a)` with `codec.decode(a).require.value`
  - Scalaz type class instances moved to the `scodec.interop.scalaz` package, in the
    [scodec-scalaz](http://github.com/scodec/scodec-scalaz) library. This library contains additional
    conveniences for Scalaz users, like syntax for converting between `Attempt[A]` and `Err \/ A`.
    In general, you should import `scodec.interop.scalaz._`.
  - Another new interop library is available as of scodec-core 1.7 -- [scodec-spire](http://github.com/scodec/scodec-spire),
    which provides codecs for unsigned numeric types.
  - `scodec.DecodingContext` is *significantly* faster. If you previously avoided using it due to performance overhead,
    retest the new implementation.
  - Dramatically improved compile time of `dropUnits`
  - Introduced new `codec.flattenLeftPairs` combinator, which converts a `Codec[(((A, B), C, ...)]` to a `Codec[A :: B :: C :: ... :: HNil]`
  - Introduced new combinators for building `Tuple3` through `Tuple12` codecs and binding the results to case classes. Calling `a ~~ b` where `a: Codec[TupleM[...]]` results in a `Codec[TupleN[..., B]]` where `N = M + 1`. The `~~` operator differs from `~` in that it returns a codec for a tuple of one higher arity than the left hand side codec. And specifically, it returns a `scodec.codecs.TupleNCodec`, where `N` is replaced by the arity. To bind a `TupleNCodec` to a case class, call `widenAs(Foo.apply, Foo.unapply)`. The advantage of using `~~` is that a `TupleN` is created directly when decoding, avoiding the nested `Tuple2` instances generated by `~`.
  - Introduced a new abstract method on `Encoder` -- `def sizeBound: SizeBound`, which provides lower and upper bounds on the size of the encoded binary. If a definition for `sizeBound` does not apply to a codec, define the method as `def sizeBound = SizeBound.unknown`. The built-combinators have been updated to compute size bounds correctly, even for product and coproduct types.

1.6.0
=====
 - `CoproductCodecBuilder` now supports arbitrary orderings of component types. This is especially useful
   when creating a `CoproductCodecBuilder` for a sealed class, because the order of subtypes in the
   coproduct representation is chosen by the compiler. See
   [example](https://github.com/scodec/scodec/blob/17f03ffe26cf91880e07a26b4c1a684918ce203a/src/test/scala/scodec/examples/CoproductsExample.scala#L227-L230).
 - Introduced the `Transform` typeclass, which abstracts the ability to `exmap` and provides derived
   implementations for `xmap`, `pxmap`, `widen`, `narrow`, and `as`, including Shapeless integration with `as`.
 - Provided instances of `Transform` for both `Codec` and  `CoproductCodecBuilder`.
 - Added various combinators to `Codec`:
   - `upcast`
   - `downcast`
   - `toFieldWithContext`
 - Introduced `KnownDiscriminatorType[D]` mixin.
 - Modified discriminated coproduct codecs and `DiscriminatorCodec` to raise
   `KnownDiscriminatorType[D]#UnknownDiscriminator` errors.
 - Added `scodec.codecs.hlist(l)` combinator for converting an `HList` of shape
   `Codec[X0] :: Codec[X1] :: ... :: Codec[Xn] :: HNil` to `Codec[X0 :: X1 :: ... :: Xn :: HNil]`.
   Also available via syntax enrichment, `toCodec`, on `HList`s of proper shape.

1.5.0
=====
 - Due to diverging implicit errors introduced by 1.4.0, the derived codec support was refactored.
   For non-generic uses, the syntax is the same -- call `Codec[Foo]` to auto-derive a codec for `Foo`.
   For generic uses, combinators should be changed to take an implicit `ImplicitCodec[A]` instead of
   an implicit `Codec[A]`. See [the issue](https://github.com/scodec/scodec/issues/30) for more
   details on the problem and [the PR](https://github.com/scodec/scodec/pull/32) for more details
   on the new structure.
 - Fixed a bug in `ushort8` where it was treating binary as a signed `short8`.
 - Added `withContext` to `Codec` -- this is an alias for `"context" | codec`.

1.4.0
=====
 - *Breaking Change* - Changed error type from String to `scodec.Err`. `Err` is a non-sealed class, allowing
   codecs to return custom subtypes describing domain specific errors. This allows dispatching on those domain
   specific errors. To upgrade, instead of returning a string, wrap the string with `Err(str)`. See
   [the PR](https://github.com/scodec/scodec/pull/27) for details.
 - Added `Codec.coproduct`, which helps create `Coproduct` based codecs.
   See [CoproductsExample](src/test/scala/scodec/examples/CoproductsExample.scala).
 - Added support for automatically deriving codecs, based on `Codec.product` and `Codec.coproduct`. See
   [DerivedCodecExamples](src/test/scala/scodec/examples/DerivedCodecExamples.scala).
 - Added `scodec.codecs.implicits`, which provides implicit codecs for primitive values and collections.
 - Updated `paddedFixedSize{Bits,Bytes}Dependent` combinator for building PKCS5/PKCS7 style padding.

1.3.2
=====
 - Added `flatAppend` and `flatConcat` to `HList` based codecs.
 - Added `Codec.product`, which automatically generates `HList` based codecs for case classes and `HList` types if all
   component types have implicit codecs available. See [ProductsExample](src/test/scala/scodec/examples/ProductsExample.scala).
 - Added the `toField` combinator to `Codec`, allowing creation of Shapeless field codecs from value codecs.
   That is, `toField[K]` on `Codec` converts a `Codec[A]` to a `Codec[FieldType[K, A]]`
 - Added `variableSize{Bits,Bytes}Long` as alternatives to `variableSize{Bits,Bytes}`, where the size is encoded/decoded
   via a `Codec[Long]` instead of a `Codec[Int]`.

1.3.1
=====
 - Added support for Shapeless coproduct codecs. See `CoproductCodecTest` for examples.
 - Added `exmap` to `Codec`, `emap` to `Decoder` and `econtramap` to `Encoder`
 - Added `narrow` and `widen` to `Codec`
 - Added ability to lift an `Encoder` to a `Codec` via `encodeOnly` and a `Decoder` to a `Codec` via `decodeOnly`
 - Added `paddedFixedSize{Bytes,Bits}` combinators
 - Added numeric codecs for `Short` and `Byte`
 - Fixed bug in `constantLenient` where decoding wasn't lenient

1.3.0
=====
 - Changed `dropLeft`/`~>` and `dropRight`/`<~` to require a `Codec[Unit]` on the dropped side.
   Previously, the requirement was a `Codec[X]` and an implicitly available `Monoid[X]`.
   To convert a `Codec[X]` to a `Codec[Unit]`, use the `unit` combinator.
 - Changed size based codecs to use `Long` instead of `Int` sizes.

1.2.2
=====
 - Added support for Shapeless coproduct codecs. See `CoproductCodecTest` for examples.
 - Added `exmap` to `Codec`, `emap` to `Decoder` and `econtramap` to `Encoder`
 - Added `narrow` and `widen` to `Codec`
 - Added ability to lift an `Encoder` to a `Codec` via `encodeOnly` and a `Decoder` to a `Codec` via `decodeOnly`
 - Added `paddedFixedSize{Bytes,Bits}` combinators
 - Added numeric codecs for `Short` and `Byte`
 - Fixed bug in `constantLenient` where decoding wasn't lenient

1.2.1
=====
 - Added `fail` combinator
 - Added `mappedEnum` combinator
 - Added `vector` and `list` combinators and deprecated `repeated`
 - Added `vectorOfN` and `listOfN` combinators for count encoded values
 - Added `unit` and `unitM` combinators to convert a `Codec[A]` to a `Codec[Unit]`
 - Added `scodec.codecs.literals._` for implicitly converting literal values to constant codecs.
 - Added `constantLenient` codec that's equivalent to `constant` but does not validate that the decoded bits equal the constant value
 - Improved support for `as` method:
   - Better compiler error message when an implicit `CodecAsAux` is not found
   - Support for reverse bindings (e.g., `Codec[Point3D].as[Int :: Int :: Int :: HNil]`)
   - Support for singleton bindings without having to first lift `Codec[A]` to `Codec[A :: HNil]` (e.g., `case class Foo(x: Int); uint8.as[Foo]`)
 - Added `optional` combinator
 - Added `withDefault` and `withDefaultValue` combinators
 - Added `recover` and `lookahead` combinators
 - Added `dropUnits` combinator for `HList` codecs, greatly simplifying creation of codecs that have unit values due to use of `constant` or `ignore`.
   For example, `(uint8 :: ignore(4) :: uint4 :: ignore(3) :: uint5).dropUnits` results in a `Codec[Int :: Int :: Int :: HNil]`
 - Added support for xmapping polymorphic functions via `polyxmap` and `polyxmap` combinators
  - `polyxmap` takes 2 polymorphic functions, forward and reverse
  - `polyxmap1` takes 1 polymorphic function that's used in both directions
  - both combinators work on `HList` codecs and value codecs

1.2.0
=====
 - Upgraded to Scalaz 7.1.0
 - Published ScalaDoc links in POM so that other projects can link to ScalaDoc

1.1.0
=====
 - Upgraded to Shapeless 2

1.0.0
=====
 - Added `endiannessDependent` combinator
 - Used `scodec.bits.ByteOrdering` instead of a boolean for indicating big endian vs little endian
 - Significant performance improvements

1.0.0-RC2
=========
 - Upgrade to scodec-bits 1.0.0-RC2

1.0.0-RC1
========
 - Added `bits` and `bytes` codecs that behave like `bits(size)`/`bytes(size)` but with no size constraint
 - Replaced discriminator support with new implementation that is much more general (thanks to Paul C)
 - Removed `Codec.{ encode, decode }` overloads that aliased `Codec#{ encode, decode }`
 - Introduced `encodeValid`, `decodeValue`, and `decodeValidValue` methods on both the `Codec` class and object
 - Added new combinators (`lazily`, `complete`, `compact`)
 - API docs

1.0.0-M3
========
 - Removed unnecessary dependencies from pom

1.0.0-M2
========
 - Changed group id from com.github.scodec to org.typelevel
 - Changed artifact id from scodec to scodec-core
 - Deprecated `scodec.{ BitVector, ByteVector }` in favor of `scodec.bits.{ BitVector, ByteVector }`
   - Deprecated forwarders will be removed in M3
 - Reduced public API
   - made many types package private
   - removed methods from `Codec` companion that existed directly on `Codec`

1.0.0-M1
========
 - JAR restructuring
   - scodec-bits: no dependency JAR containing BitVector, ByteVector, and supporting types
   - scodec: dependds on scodec-bits and adds encoding/decoding capabilities
   - See scodec-bits for list of improvements to BitVector and ByteVector
 - Package restructuring
   - scodec.bits package contains BitVector, ByteVector, and supporting types
   - scodec package contains main abstractions of encoding/decoding
   - scodec.codecs package contains reusable codecs
 - Encoder, Decoder, and GenCodec abstractions, which allow simpler transforms if a full Codec is not required
