package lzw

import java.io.OutputStream

class OutputBitStream(outputStream: OutputStream, packingOrder: PackingOrder) {
  def put(bits: BitString): Unit = ???
  def close(): Unit = ???
}
