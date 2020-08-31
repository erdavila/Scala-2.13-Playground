package lzw

import java.io.InputStream

class InputBitStream(inputStream: InputStream, packingOrder: PackingOrder) {
  def get(count: Int): BitString = ???
}
