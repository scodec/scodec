package scodec

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

private[scodec] object Platform {
  val utf8: Charset = StandardCharsets.UTF_8
  val ascii: Charset = StandardCharsets.US_ASCII
}
