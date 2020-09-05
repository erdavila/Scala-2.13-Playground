package lzw

class BitStringEncoder(packingOrderFirst: BitSignificance) {
  private var buffer = BitString.empty

  def putBytes(bytes: Array[Byte]): Unit =
    buffer = bytes.foldLeft(buffer) { (buf, byte) =>
      buf.end(packingOrderFirst).otherEnd.extend(BitString.from(byte))
    }

  def bitsAvailable: Int =
    buffer.length

  def getBits(n: Int): BitString = {
    val (newBuffer, bits) = buffer.end(packingOrderFirst).splitAt(n)
    buffer = newBuffer
    bits
  }
}
