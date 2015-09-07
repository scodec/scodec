package scodec

import java.nio.charset.Charset

private[scodec] object Platform {
  val utf8: Charset = Charset.forName("UTF-8")
  val ascii: Charset = Charset.forName("US-ASCII")
}
