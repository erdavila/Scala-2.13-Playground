package lzw.streams

import java.io.InputStream
import lzw.bytes.{LzwByteEncoder, Options}
import scala.annotation.tailrec

class LzwEncoderInputStream(in: InputStream, options: Options) extends InputStream {
  private val lzwByteEncoder = new LzwByteEncoder(options)
  private var finished: Boolean = false
  private val buffer = new ByteArrayBuffer
  private val reusableSingleByteBuffer = new Array[Byte](1)

  private def readAheadBufferLength(requestedLen: Int): Int = requestedLen * 3 / 2

  override def read(): Int =
    read(reusableSingleByteBuffer) match {
      case -1 => -1
      case _ => reusableSingleByteBuffer(0) & 0x000000FF
    }

  override def read(b: Array[Byte]): Int =
    read(b, 0, b.length)

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    if (len == 0) {
      0
    } else {
      val copied = buffer.copyTo(b, off, len)
      if (copied > 0) {
        copied
      } else if (finished) {
        -1
      } else {
        val readBuffer = new Array[Byte](readAheadBufferLength(len))

        @tailrec
        def loop(): Int = {
          val readCount = in.read(readBuffer)
          if (readCount == -1) {
            finished = true
            buffer.set(lzwByteEncoder.finish())
            val copied = buffer.copyTo(b, off, len)
            if (copied > 0) {
              copied
            } else {
              -1
            }
          } else {
            buffer.set(lzwByteEncoder.encode(readBuffer.take(readCount)))
            val copied = buffer.copyTo(b, off, len)
            if (copied > 0) {
              copied
            } else {
              loop()
            }
          }
        }

        loop()
      }
    }

  def resetEncoding(): Unit =
    buffer.append(lzwByteEncoder.reset())

  def dictionarySize: Int = lzwByteEncoder.statistics.dictionarySize

  def maxCodeWidthExhausted: Boolean = lzwByteEncoder.maxCodeWidthExhausted
}
