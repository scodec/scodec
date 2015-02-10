package scodec
package codecs

/** Mixin for codecs/decoders that are known to discriminate by values of type `D`. */
trait KnownDiscriminatorType[D] {
  /** Error raised when an unknown discriminator is encountered when decoding. */
  case class UnknownDiscriminator(discriminator: D, context: List[String]) extends Err {
    def this(discriminator: D) = this(discriminator, Nil)
    def message = s"Unknown discriminator $discriminator"
    def pushContext(ctx: String) = copy(context = ctx :: context)
  }
}

