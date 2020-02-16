/**
  * Combinator library for working with binary data.
  *
  * The primary abstraction of this library is [[Codec]], which provides the ability to encode/decode values to/from binary.
  * [[Codec]] extends both [[Encoder]] and [[Decoder]].
  *
  * See the [[codecs]] package object for pre-defined codecs for many common data types and combinators for building larger
  * codecs out of smaller ones.
  *
  * For the categorically minded, note the following:
  *  - `Decoder` is a monad
  *  - `Encoder` is a contravariant functor
  *  - `Codec` is an invariant functor
  */
package object scodec
