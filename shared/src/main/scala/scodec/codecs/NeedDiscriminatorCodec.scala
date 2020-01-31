package scodec
package codecs

/**
  * Supports creation of a [[DiscriminatorCodec]]. See [[discriminated]] for details.
  * @group combinators
  */
abstract class NeedDiscriminatorCodec[A] {
  def by[B](discriminatorCodec: Codec[B]): DiscriminatorCodec[A, B]
}
