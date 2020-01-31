package scodec
package codecs

import scodec.bits._
import shapeless.Lazy

import java.util.UUID

/** Provides implicit codecs for simple value types. */
trait ImplicitValues {
  implicit val implicitByteCodec: Codec[Byte] = byte
  implicit val implicitShortCodec: Codec[Short] = short16
  implicit val implicitIntCodec: Codec[Int] = int32
  implicit val implicitLongCodec: Codec[Long] = int64
  implicit val implicitFloatCodec: Codec[Float] = float
  implicit val implicitDoubleCodec: Codec[Double] = double
  implicit val implicitStringCodec: Codec[String] = utf8_32
  implicit val implicitBooleanCodec: Codec[Boolean] = bool(8)
  implicit val implicitBitVectorCodec: Codec[BitVector] = variableSizeBitsLong(int64, bits)
  implicit val implicitByteVectorCodec: Codec[ByteVector] = variableSizeBytesLong(int64, bytes)
  implicit val implicitUuidCodec: Codec[UUID] = uuid
}

/** Provides implicit codecs for collection types. */
trait ImplicitCollections {
  implicit def implicitListCodec[A](
      implicit ccount: Lazy[Codec[Int]],
      ca: Lazy[Codec[A]]
  ): Codec[List[A]] = listOfN(ccount.value, ca.value)
  implicit def implicitVectorCodec[A](
      implicit ccount: Lazy[Codec[Int]],
      ca: Lazy[Codec[A]]
  ): Codec[Vector[A]] = vectorOfN(ccount.value, ca.value)
  implicit def implicitOptionCodec[A](
      implicit cguard: Lazy[Codec[Boolean]],
      ca: Lazy[Codec[A]]
  ): Codec[Option[A]] = optional(cguard.value, ca.value)
}

/** Provides implicit codecs for common types. */
trait ImplicitCodecs extends ImplicitValues with ImplicitCollections
