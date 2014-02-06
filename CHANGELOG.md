1.0.0-m8
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
