package scodec

object ByteVector {

  def apply(bytes: Byte*): ByteVector = Vector(bytes: _*)
}
