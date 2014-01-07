package scodec


class NamedCodec[A](name: String, target: Codec[A]) extends Codec[A] {

  override def encode(a: A) =
    target.encode(a).leftMap { e => s"$name: $e" }

  override def decode(buffer: BitVector) =
    target.decode(buffer).leftMap { e => s"$name: $e" }

  override def toString = s"$name($target)"

}

private[scodec] trait NamedCodecSyntax {

  final implicit class StringEnrichedWithCodecNamingSupport(val name: String) {
    /** Names the specified codec, resulting in the name being included in error messages. */
    def |[A](codec: Codec[A]): Codec[A] = new NamedCodec(name, codec)
  }

}
