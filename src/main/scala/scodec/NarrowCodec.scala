package scodec

//import scala.reflect.runtime.universe.{TypeTag, typeTag}

/*
class NarrowCodec[A, B <: A : TypeTag](codec: Codec[B]) extends Codec[A] {

  override def encode(value: A) = {

    classTag[B].tpe

  }

  override def decode(buffer: BitVector) = ???
}
*/
