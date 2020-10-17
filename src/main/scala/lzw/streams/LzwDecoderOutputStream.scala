package lzw.streams

import java.io.OutputStream
import lzw.bytes.{LzwByteDecoder, Options}

class LzwDecoderOutputStream(out: OutputStream, options: Options) extends OutputStream {
  private val lzwByteDecoder = new LzwByteDecoder(options)

  override def write(b: Int): Unit =
    out.write(lzwByteDecoder.decode(Array(b.toByte)))

  override def write(b: Array[Byte]): Unit =
    out.write(lzwByteDecoder.decode(b))

  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    out.write(lzwByteDecoder.decode(b.slice(off, off + len)))
}
