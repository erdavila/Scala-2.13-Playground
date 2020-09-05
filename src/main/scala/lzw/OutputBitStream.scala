package lzw

import java.io.OutputStream

class OutputBitStream(outputStream: OutputStream, packingOrderFirst: BitSignificance) {
  private var bufferedBits = BitString.empty

  def put(bits: BitString): Unit = {
    bufferedBits = bufferedBits.end(packingOrderFirst).otherEnd.extend(bits)
    val fullBytes = bufferedBits.length / java.lang.Byte.SIZE
    if (fullBytes > 0) {
      val (newBufferedBits, bitsToWrite) = bufferedBits.end(packingOrderFirst).splitAt(fullBytes * java.lang.Byte.SIZE)
      bufferedBits = newBufferedBits
      val bytes = bitsToWrite.end(packingOrderFirst).bytes.toArray
      outputStream.write(bytes)
    }
  }

  def close(): Unit = {
    if (bufferedBits.length > 0) {
      assert(bufferedBits.length < java.lang.Byte.SIZE)
      val padded = bufferedBits.end(packingOrderFirst).otherEnd.padTo(java.lang.Byte.SIZE)
      val bytes = padded.lsb.bytes.toArray
      outputStream.write(bytes)
    }
  }
}
