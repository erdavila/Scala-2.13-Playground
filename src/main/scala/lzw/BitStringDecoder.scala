package lzw

import scala.annotation.tailrec

class BitStringDecoder(packingOrder: PackingOrder) {
  private var buffer = BitString.empty

  def decode(bits: BitString): Array[Byte] = {
    buffer = packingOrder(buffer).otherEnd.extend(bits)

    @tailrec
    def loop(bytes: Array[Byte]): Array[Byte] =
      if (buffer.length >= java.lang.Byte.SIZE) {
        val (newBuffer, byteBits) = packingOrder(buffer).splitAt(java.lang.Byte.SIZE)
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
      val byteBits = packingOrder(buffer).otherEnd.padTo(java.lang.Byte.SIZE)
      byteBits.lsb.bytes.toSeq.head
    }
}
