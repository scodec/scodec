package scodec
package codecs

import java.util.zip.Deflater

private[scodec] trait codecsplatform {

  /**
    * Codec that compresses the results of encoding with the specified codec and decompresses prior to decoding with the specified codec.
    *
    * Compression is performed using ZLIB. There are a number of defaulted parameters that control compression specifics.
    *
    * @param level compression level, 0-9, with 0 disabling compression and 9 being highest level of compression -- see `java.util.zip.Deflater` for details
    * @param strategy compression strategy -- see `java.util.zip.Deflater` for details
    * @param nowrap if true, ZLIB header and checksum will not be used
    * @param chunkSize buffer size, in bytes, to use when compressing
    * @group combinators
    */
  def zlib[A](
      codec: Codec[A],
      level: Int = Deflater.DEFAULT_COMPRESSION,
      strategy: Int = Deflater.DEFAULT_STRATEGY,
      nowrap: Boolean = false,
      chunkSize: Int = 4096
  ): Codec[A] =
    new ZlibCodec(codec, level, strategy, nowrap, chunkSize)
}
