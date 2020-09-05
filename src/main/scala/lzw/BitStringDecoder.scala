package lzw

import scala.annotation.tailrec

class BitStringDecoder(packingOrderFirst: BitSignificance) {
  private var buffer = BitString.empty

  def decode(bits: BitString): Array[Byte] = {
    buffer = buffer.end(packingOrderFirst).otherEnd.extend(bits)

    @tailrec
    def loop(bytes: Array[Byte]): Array[Byte] =
      if (buffer.length >= java.lang.Byte.SIZE) {
        val (newBuffer, byteBits) = buffer.end(packingOrderFirst).splitAt(java.lang.Byte.SIZE)
        buffer = newBuffer
        loop(bytes :+ byteBits.lsb.bytes.toSeq.head)
      } else {
        bytes
      }

    loop(Array.empty)
  }

  def finish(): Option[Byte] =
    Option.when(buffer.length > 0) {
      assert(buffer.length < java.lang.Byte.SIZE)
      val byteBits = buffer.end(packingOrderFirst).otherEnd.padTo(java.lang.Byte.SIZE)
      byteBits.lsb.bytes.toSeq.head
    }
}
