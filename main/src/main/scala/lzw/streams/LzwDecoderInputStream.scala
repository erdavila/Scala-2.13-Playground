package lzw.streams

import java.io.InputStream
import lzw.bytes.{LzwByteDecoder, Options}
import scala.annotation.tailrec

class LzwDecoderInputStream(in: InputStream, options: Options) extends InputStream {
  private val lzwByteDecoder = new LzwByteDecoder(options)
  private var finished: Boolean = false
  private val buffer = new ByteArrayBuffer
  private val reusableSingleByteBuffer = new Array[Byte](1)

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
        val readBuffer = new Array[Byte](len)

        @tailrec
        def loop(): Int = {
          val readCount = in.read(readBuffer)
          if (readCount == -1) {
            finished = true
            -1
          } else {
            buffer.set(lzwByteDecoder.decode(readBuffer.take(readCount)))
            finished = lzwByteDecoder.stopped
            val copied = buffer.copyTo(b, off, len)
            if (copied > 0) {
              copied
            } else if (finished) {
              -1
            } else {
              loop()
            }
          }
        }

        loop()
      }
    }

  def stopped: Boolean = lzwByteDecoder.stopped
}
