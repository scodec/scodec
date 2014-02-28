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
