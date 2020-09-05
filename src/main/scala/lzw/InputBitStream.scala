package lzw

import java.io.InputStream
import scala.annotation.tailrec

class InputBitStream(inputStream: InputStream, packingOrderFirst: BitSignificance) {

  private var bufferedBits: BitString = BitString.empty

  def get(count: Int): BitString = {
    if (count > bufferedBits.length) {
      val bytesRequired = BitString.requiredUnitsForLength(count - bufferedBits.length, java.lang.Byte.SIZE)
      bufferedBits = readBytes(bytesRequired)
        .map(BitString.from)
        .foldLeft(bufferedBits)(_.end(packingOrderFirst).otherEnd.extend(_))
    }

    val (newBufferedBits, result) = bufferedBits.end(packingOrderFirst).splitAt(count)
    bufferedBits = newBufferedBits
    result
  }

  private def readBytes(count: Int): Array[Byte] = {
    val bytes = new Array[Byte](count)

    @tailrec
    def loop(offset: Int): Int = {
      val toRead = bytes.length - offset
      val wereRead = inputStream.read(bytes, offset, toRead)
      if (wereRead == -1) {
        offset
      } else if (wereRead < toRead) {
        loop(offset + wereRead)
      } else {
        offset + wereRead
      }
    }

    val wereRead = loop(0)
    bytes.take(wereRead)
  }
}
