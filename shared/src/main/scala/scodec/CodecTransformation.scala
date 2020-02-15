package scodec

/** Universally quantified transformation of a `Codec` to a `Codec`. */
type CodecTransformation = [x] => Codec[x] => Codec[x]

/** Companion for [[CodecTransformation]]. */
object CodecTransformation extends Serializable {
  val Id: CodecTransformation = [x] => (c: Codec[x]) => c
}