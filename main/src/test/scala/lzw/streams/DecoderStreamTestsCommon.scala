package lzw.streams

import lzw.bytes.LzwByteEncoder

trait DecoderStreamTestsCommon extends StreamTestsCommon {
  protected val encodedContent: Array[Byte] = {
    val encoder = new LzwByteEncoder(options)
    val unfinishedBytes = encoder.encode(decodedBytesInput)
    unfinishedBytes ++ encoder.finish()
  }
}
