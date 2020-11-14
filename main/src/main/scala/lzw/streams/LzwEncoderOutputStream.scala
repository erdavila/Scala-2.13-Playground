package lzw.streams

import java.io.OutputStream
import lzw.bytes.{LzwByteEncoder, Options}

class LzwEncoderOutputStream(out: OutputStream, options: Options) extends OutputStream {
  private val lzwByteEncoder = new LzwByteEncoder(options)

  override def write(b: Int): Unit =
    this.write(Array(b.toByte))

  override def write(b: Array[Byte]): Unit = {
    val encodedBytes = lzwByteEncoder.encode(b)
    out.write(encodedBytes)
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    this.write(b.slice(off, off + len))

  def resetEncoding(): Unit = {
    val encodedBytes = lzwByteEncoder.reset()
    out.write(encodedBytes)
  }

  override def close(): Unit = {
    val encodedBytes = lzwByteEncoder.finish()
    out.write(encodedBytes)
    out.close()
  }

  def dictionarySize: Int = lzwByteEncoder.statistics.dictionarySize

  def maxCodeWidthExhausted: Boolean = lzwByteEncoder.maxCodeWidthExhausted
}
