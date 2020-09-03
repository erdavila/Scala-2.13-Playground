package lzw

class BitStringEncoder(packingOrder: PackingOrder) {
  private var buffer = BitString.empty

  def putBytes(bytes: Array[Byte]): Unit =
    buffer = bytes.foldLeft(buffer) { (buf, byte) =>
      packingOrder(buf).otherEnd.extend(BitString.from(byte))
    }

  def bitsAvailable: Int =
    buffer.length

  def getBits(n: Int): BitString = {
    val (newBuffer, bits) = packingOrder(buffer).splitAt(n)
    buffer = newBuffer
    bits
  }
}
