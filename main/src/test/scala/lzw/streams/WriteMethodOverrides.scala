package lzw.streams

import java.io.OutputStream

class WriteMethodOverrides(out: OutputStream, writeBufferSize: Int, bytes: Array[Byte]) {
  require(writeBufferSize >= 1)
  private var _position: Int = 0

  def position: Int = _position

  sealed trait Override {
    def execute(): Boolean
    def next: Override
  }

  val singleByte: Override = new Override {
    override def execute(): Boolean = {
      whenRemainingBytes(1) { _ =>
        val byte = bytes(_position)
        out.write(byte)
      }
    }

    override def next: Override = byteArray
  }

  val byteArray: Override = new Override {
    private val writeBuffer = new Array[Byte](writeBufferSize)

    override def execute(): Boolean =
      whenRemainingBytes(writeBufferSize) { count =>
        val buffer = if (count == writeBuffer.length) writeBuffer else new Array[Byte](count)
        Array.copy(bytes, _position, buffer, 0, count)
        out.write(buffer)
      }

    override def next: Override = byteArraySection
  }

  val byteArraySection: Override = new Override {
    override def execute(): Boolean =
      whenRemainingBytes(writeBufferSize) { count =>
        out.write(bytes, _position, count)
      }

    override def next: Override = singleByte
  }

  @inline private def whenRemainingBytes(n: Int)(f: Int => Any): Boolean = {
    val remainingBytes = bytes.length - _position
    val hasRemainingBytes = remainingBytes > 0
    if (hasRemainingBytes) {
      val count = math.min(n, remainingBytes)
      f(count)
      _position += count
    }
    hasRemainingBytes
  }
}
