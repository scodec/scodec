/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec
package codecs

import java.util.zip.Deflater

/**
  * Codec that compresses the results of encoding with the specified codec and decompresses prior to decoding with the specified codec.
  *
  * Compression is performed using ZLIB. There are a number of defaulted parameters that control compression specifics.
  *
  * @param level compression level, 0-9, with 0 disabling compression and 9 being highest level of compression -- see `java.util.zip.Deflater` for details
  * @param strategy compression strategy -- see `java.util.zip.Deflater` for details
  * @param nowrap if true, ZLIB header and checksum will not be used
  * @param chunkSize buffer size, in bytes, to use when compressing
  */
def zlib[A](
    codec: Codec[A],
    level: Int = Deflater.DEFAULT_COMPRESSION,
    strategy: Int = Deflater.DEFAULT_STRATEGY,
    nowrap: Boolean = false,
    chunkSize: Int = 4096
): Codec[A] =
  new ZlibCodec(codec, level, strategy, nowrap, chunkSize)


