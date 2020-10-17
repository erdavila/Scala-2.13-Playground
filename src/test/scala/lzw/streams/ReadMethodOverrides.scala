package lzw.streams

import java.io.InputStream

class ReadMethodOverrides(in: InputStream, readBufferSize: Int, bytesConsumer: Array[Byte] => Any) {
  require(readBufferSize >= 0)
  private val Gap = 2
  private val readBuffer = new Array[Byte](readBufferSize + 2 * Gap)

  sealed trait Override {
    def execute(): Boolean
    def next: Override
  }

  val singleByte: Override = new Override {
    val reusableSingleByteArray = new Array[Byte](1)

    override def execute(): Boolean =
      whenNotEnded(in.read()) { byteValue =>
        reusableSingleByteArray(0) = byteValue.toByte
        reusableSingleByteArray
      }

    override def next: Override = byteArray
  }

  val byteArray: Override = new Override {
    override def execute(): Boolean =
      whenNotEnded(in.read(readBuffer)) { byteCount =>
        if (byteCount < readBuffer.length) {
          readBuffer.take(byteCount)
        } else {
          readBuffer
        }
      }

    override def next: Override = byteArraySection
  }

  val byteArraySection: Override = new Override {
    override def execute(): Boolean =
      whenNotEnded(in.read(readBuffer, Gap, readBuffer.length - 2 * Gap)) { byteCount =>
        readBuffer.slice(Gap, Gap + byteCount)
      }

    override def next: Override = singleByte
  }


  @inline private def whenNotEnded(i: Int)(f: Int => Array[Byte]): Boolean = {
    val stillReading = i != -1
    if (stillReading) {
      val bytes = f(i)
      bytesConsumer(bytes)
    }
    stillReading
  }
}
