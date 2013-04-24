package scodec


class NamedCodec[A](name: String, target: Codec[A]) extends Codec[A] {

  override def encode(a: A) =
    target.encode(a).leftMap { e => s"$name: $e" }

  override def decode(buffer: BitVector) =
    target.decode(buffer).leftMap { e => s"$name: $e" }

}

trait NamedCodecSyntax {

  implicit class StringEnrichedWithCodecNamingSupport(name: String) {
    def |[A](codec: Codec[A]): Codec[A] = new NamedCodec(name, codec)
  }

}
