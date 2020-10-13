package lzw.streams

private class ByteArrayBuffer {
  private var content: Array[Byte] = Array.emptyByteArray
  private var position: Int = 0

  def set(bytes: Array[Byte]): Unit = {
    content = bytes
    position = 0
  }

  def append(bytes: Array[Byte]): Unit =
    content = content ++ bytes

  def copyTo(array: Array[Byte], start: Int, len: Int): Int = {
    val copied = Array(len, content.length - position, array.length - start).min
    Array.copy(content, position, array, start, copied)
    position += copied
    copied
  }
}
