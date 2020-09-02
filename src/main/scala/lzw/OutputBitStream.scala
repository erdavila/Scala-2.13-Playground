package lzw

import java.io.OutputStream

class OutputBitStream(outputStream: OutputStream, packingOrder: PackingOrder) {
  private var bufferedBits = BitString.empty

  def put(bits: BitString): Unit = {
    bufferedBits = packingOrder(bufferedBits).otherEnd.extend(bits)
    val fullBytes = bufferedBits.length / java.lang.Byte.SIZE
    if (fullBytes > 0) {
      val (newBufferedBits, bitsToWrite) = packingOrder(bufferedBits).splitAt(fullBytes * java.lang.Byte.SIZE)
      bufferedBits = newBufferedBits
      val bytes = packingOrder(bitsToWrite).bytes.toArray
      outputStream.write(bytes)
    }
  }

  def close(): Unit = {
    if (bufferedBits.length > 0) {
      assert(bufferedBits.length < java.lang.Byte.SIZE)
      val padded = packingOrder(bufferedBits).otherEnd.padTo(java.lang.Byte.SIZE)
      val bytes = padded.lsb.bytes.toArray
      outputStream.write(bytes)
    }
  }
}
